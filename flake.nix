{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    flake-root.url = "github:srid/flake-root";
    fourmolu-nix.url = "github:jedimahdi/fourmolu-nix";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.fourmolu-nix.flakeModule
      ];
      perSystem = { self', system, lib, config, pkgs, ... }: {
        haskellProjects.main = {
          # packages.auspex.root = ./.;  # Auto-discovered by haskell-flake
          devShell = {
            hlsCheck.enable = false;
          };

          settings =
            let
              unbreak = { jailbreak = true; broken = false; };
            in
            { };

          autoWire = [ "packages" "apps" "checks" ];
        };

        treefmt.config = {
          projectRootFile = "flake.nix";

          programs.fourmolu.enable = true;
          programs.fourmolu.package = config.fourmolu.wrapper;
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;

          settings.formatter.hlint = {
            command = pkgs.hlint;
            options = [ "-j" "-X" "QuasiQuotes" "-X" "TemplateHaskell" ];
            includes = [ "*.hs" ];
          };
        };

        fourmolu.settings = {
          indentation = 2;
          comma-style = "leading";
          column-limit = 80;
          record-brace-space = true;
          indent-wheres = true;
          import-export-style = "diff-friendly";
          respectful = true;
          haddock-style = "multi-line";
          newlines-between-decls = 1;
          extensions = [ "ImportQualifiedPost" ];
        };

        packages.default = self'.packages.main-auspex;
        packages.auspex-docker = pkgs.dockerTools.buildImage {
          name = "auspex-docker";

          config = {
            Cmd = [ "auspex" ];
            ExposedPorts = {
              "8080/tcp" = { };
            };
          };

          copyToRoot = pkgs.buildEnv {
            name = "image-root";
            paths = with pkgs; [
              self'.packages.main-auspex
            ];
            pathsToLink = [ "/bin" ];
          };
        };

        devShells.default = pkgs.mkShell {
          name = "haskell-template";
          meta.description = "Haskell development environment";
          inputsFrom = [
            config.haskellProjects.main.outputs.devShell
            config.treefmt.build.devShell
          ];
          packages = with pkgs; [
            just
          ];
        };

        checks = {
          build = self'.packages.default;
          test = import ./nix/test.nix inputs;
        };

      };

      flake = {
        nixosModules.default = import ./nix/module.nix inputs;


        hydraJobs.x86_64-linux = self.checks.x86_64-linux;
      };
    };
}

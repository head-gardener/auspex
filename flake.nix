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
            hlsCheck.enable = true;
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
          programs.hlint.enable = true;
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
      };
    };
}

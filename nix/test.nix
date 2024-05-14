inputs:

let
  system = "x86_64-linux";
  pkgs = inputs.nixpkgs.legacyPackages.${system};
in

with import (inputs.nixpkgs + "/nixos/lib/testing-python.nix")
{
  inherit system;
};

makeTest {
  name = "auspex";

  nodes = {
    client = { ... }: {
      networking.firewall.allowedTCPPorts = [ 3000 ];
      environment.systemPackages = with pkgs; [
        inputs.self.packages.${system}.default
      ];
    };

    owner = { ... }: {
      environment.systemPackages = with pkgs; [
        inputs.self.packages.${system}.default
        openssh
      ];
    };

    provider = { ... }: {
      imports = [ inputs.self.nixosModules.default ];
      services.auspex = {
        enable = true;
        openFirewall = true;
        port = 8090;
      };
    };
  };

  testScript = ''
    start_all()
    provider.wait_for_unit("multi-user.target")

    client.wait_for_unit("multi-user.target")
    client.execute("systemd-run example-app http://provider:8090")

    owner.wait_for_unit("multi-user.target")
    owner.succeed("curl provider:8090")
    owner.succeed("curl client:3000")
    owner.succeed("ssh-keygen -t ed25519 -f ./ed25519 -N \"\"")
    owner.succeed("auspex-client user http://provider:8090 ed25519 -r")
    owner.succeed("auspex-client user http://provider:8090 ed25519")
    owner.succeed("curl -f \"client:3000/auth/page/auspex/login?token=$(auspex-client user http://provider:8090 ed25519)\"")
  '';
}

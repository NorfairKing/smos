{ pkgs ? import ./pkgs.nix { static = false; } }:
let
  smos-production = import ./nixos-module.nix { envname = "production"; };
  home-manager = import (
    builtins.fetchTarball {
      url = "https://github.com/rycee/home-manager/archive/472ca211cac604efdf621337067a237be9df389e.tar.gz";
      sha256 = "sha256:1gbfsnd7zsxwqryxd4r6jz9sgdz6ghlkapws1cdxshrbxlwhqad1";
    } + "/nixos/default.nix"
  );

  docs-port = 8001;
  api-port = 8002;
  web-port = 8003;
in
pkgs.nixosTest (
  { lib, pkgs, ... }: {
    name = "smos-module-test";
    machine = {
      imports = [
        smos-production
        home-manager
      ];
      services.smos.production = {
        enable = true;
        docs-site = {
          enable = true;
          port = docs-port;
        };
        api-server = {
          enable = true;
          port = api-port;
        };
        web-server = {
          enable = true;
          port = web-port;
          api-url = "localhost:${builtins.toString api-port}";
        };
      };
      users.users.testuser.isNormalUser = true;
      home-manager.users.testuser = { pkgs, ... }: {
        imports = [
          ./home-manager-module.nix
        ];
        xdg.enable = true;
        home.stateVersion = "20.09"; # To make sure it actualy use the
        programs.smos = {
          enable = true;
          backup.enable = true;
          sync = {
            enable = true;
            server-url = "localhost:${builtins.toString api-port}";
            username = "testuser";
            password = "testpassword";
          };
          scheduler = {
            enable = true;
          };
        };
      };
    };
    testScript = ''
      from shlex import quote

      machine.start()
      machine.wait_for_unit("multi-user.target")

      machine.wait_for_open_port(${builtins.toString docs-port})
      machine.succeed("curl localhost:${builtins.toString docs-port}")
      machine.wait_for_open_port(${builtins.toString api-port})
      machine.succeed("curl localhost:${builtins.toString api-port}")
      machine.wait_for_open_port(${builtins.toString web-port})
      machine.succeed("curl localhost:${builtins.toString web-port}")

      machine.wait_for_unit("home-manager-testuser.service")


      def su(user, cmd):
          return f"su - {user} -c {quote(cmd)}"


      machine.succeed(su("testuser", "cat ~/.config/smos/config.yaml"))

      # Sync
      machine.succeed(su("testuser", "smos-sync-client register"))
      machine.succeed(su("testuser", "smos-sync-client login"))
      machine.succeed(su("testuser", "smos-sync-client sync"))

      # Calendar import
      machine.succeed(su("testuser", "smos-scheduler schedule"))
    '';
  }
)

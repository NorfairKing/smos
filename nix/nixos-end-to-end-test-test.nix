{ pkgs ? import ./pkgs.nix { static = false; } }:
let
  # See this for more info:
  # https://github.com/NixOS/nixpkgs/blob/99d379c45c793c078af4bb5d6c85459f72b1f30b/nixos/lib/testing-python.nix
  smos-testing = import ./nixos-module.nix { envname = "testing"; };
  smos-end-to-end-testing = import ./end-to-end-test-nixos-module.nix;

  docs-port = 8001;
  api-port = 8002;
  web-port = 8003;

in
pkgs.nixosTest (
  { lib, pkgs, ... }: {
    name = "smos-module-test";
    nodes = {
      server = {
        imports = [
          smos-testing
        ];
        services.smos.testing = {
          enable = true;
          docs-site = {
            enable = true;
            port = docs-port;
          };
          api-server = {
            enable = true;
            port = api-port;
            admin = "admin";
            auto-backup = {
              enable = true;
              phase = 1;
              period = 5;
            };
            backup-garbage-collector = {
              enable = false;
            };
          };
          web-server = {
            enable = true;
            port = web-port;
            api-url = "server:${builtins.toString api-port}";
          };
        };
      };
      client = {
        imports = [
          smos-end-to-end-testing
        ];
        services.smos.end-to-end-testing = {
          enable = true;
          api-server = {
            enable = true;
            api-url = "server:${builtins.toString api-port}";
          };
        };
      };
    };
    testScript = ''
      from shlex import quote

      server.start()
      client.start()
      server.wait_for_unit("multi-user.target")
      client.wait_for_unit("multi-user.target")

      server.wait_for_open_port(${builtins.toString docs-port})
      client.succeed("curl server:${builtins.toString docs-port}")
      server.wait_for_open_port(${builtins.toString api-port})
      client.succeed("curl server:${builtins.toString api-port}")
      server.wait_for_open_port(${builtins.toString web-port})
      client.succeed("curl server:${builtins.toString web-port}")

      print("starting end-to-end-tests")
      client.systemctl("start smos-server-end-to-end-test.service --wait")
      print("end-to-end-tests done")
    '';
  }
)

{ pkgs ? import ./pkgs.nix { static = false; } }:
let
  # See this for more info:
  # https://github.com/NixOS/nixpkgs/blob/99d379c45c793c078af4bb5d6c85459f72b1f30b/nixos/lib/testing-python.nix
  smos-production = import ./nixos-module.nix { envname = "production"; };
  home-manager = import (
    builtins.fetchTarball
      {
        url = "https://github.com/rycee/home-manager/archive/472ca211cac604efdf621337067a237be9df389e.tar.gz";
        sha256 = "sha256:1gbfsnd7zsxwqryxd4r6jz9sgdz6ghlkapws1cdxshrbxlwhqad1";
      } + "/nixos/default.nix"
  );

  docs-port = 8001;
  api-port = 8002;
  web-port = 8003;

  testUsers = {
    "nothing_enabled" = {
      programs.smos = {
        enable = true;
      };
    };
    "backup_enabled" = {
      programs.smos = {
        enable = true;
        backup.enable = true;
      };
    };
    "sync_enabled" = {
      programs.smos = {
        enable = true;
        sync = {
          enable = true;
          server-url = "localhost:${builtins.toString api-port}";
          username = "sync_enabled";
          password = "testpassword";
        };
      };
    };
    "scheduler_enabled" = {
      programs.smos = {
        enable = true;
        scheduler.enable = true;
      };
    };
    "everything_enabled" = {
      programs.smos = {
        enable = true;
        backup.enable = true;
        sync = {
          enable = true;
          server-url = "localhost:${builtins.toString api-port}";
          username = "everything_enabled";
          password = "testpassword";
        };
        scheduler = {
          enable = true;
        };
      };
    };
  };
  makeTestUser = _: _: {
    isNormalUser = true;
  };
  makeTestUserHome = username: userConfig: { pkgs, ... }:
    userConfig // {
      imports = [
        ./home-manager-module.nix
      ];
      xdg.enable = true;
      home.stateVersion = "20.09";
    };

  # The strange formatting is because of the stupid linting that nixos tests do
  commonTestScript = username: userConfig: pkgs.lib.optionalString (userConfig.programs.smos.enable or false) ''

    # Test that the config file exists.
    machine.succeed(su("${username}", "cat ~/.config/smos/config.yaml"))
    # Make sure the user can run the smos commands.
    machine.succeed(su("${username}", "smos --help"))
    machine.succeed(su("${username}", "smos-archive --help"))
    machine.succeed(su("${username}", "smos-calendar-import --help"))
    machine.succeed(su("${username}", "smos-convert-org --help"))
    machine.succeed(su("${username}", "smos-query --help"))
    machine.succeed(su("${username}", "smos-scheduler --help"))
    machine.succeed(su("${username}", "smos-sync-client --help"))
    # Make sure the config file is parseable
    machine.succeed(su("${username}", "smos-query next"))'';

  syncTestScript = username: userConfig: pkgs.lib.optionalString (userConfig.programs.smos.sync.enable or false) ''

    # Test that syncing works.
    machine.succeed(su("${username}", "smos-sync-client register"))
    machine.succeed(su("${username}", "smos-sync-client login"))
    machine.succeed(su("${username}", "smos-sync-client sync"))'';

  schedulerTestScript = username: userConfig: pkgs.lib.optionalString (userConfig.programs.smos.scheduler.enable or false) ''

    # Test that the scheduler can activate.
    machine.succeed(su("${username}", "smos-scheduler check"))
    machine.succeed(su("${username}", "smos-scheduler schedule"))'';

  userTestScript = username: userConfig: pkgs.lib.concatStrings [
    (commonTestScript username userConfig)
    (syncTestScript username userConfig)
    (schedulerTestScript username userConfig)
  ];

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
      users.users = lib.mapAttrs makeTestUser testUsers;
      home-manager.users = lib.mapAttrs makeTestUserHome testUsers;
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

      # Wait for all test users
      ${lib.concatStringsSep "\n" (builtins.map (username: "machine.wait_for_unit(\"home-manager-${username}.service\")") (builtins.attrNames testUsers))}
      

      def su(user, cmd):
          return f"su - {user} -c {quote(cmd)}"


      # Run the test script for each user
      ${lib.concatStrings (lib.mapAttrsToList userTestScript testUsers)}
    '';
  }
)

{ sources ? import ./sources.nix
, pkgs ? import ./pkgs.nix { inherit sources; static = false; }
, smosPackages ? pkgs.smosPackages
}:
let
  # See this for more info:
  # https://github.com/NixOS/nixpkgs/blob/99d379c45c793c078af4bb5d6c85459f72b1f30b/nixos/lib/testing-python.nix
  smos-production = import ./nixos-module.nix {
    inherit sources;
    inherit pkgs;
    inherit smosPackages;
    envname = "production";
  };
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
      programs.smos = { };
    };
    "backup_enabled" = {
      programs.smos = {
        backup.enable = true;
      };
    };
    "sync_enabled" = {
      programs.smos = {
        sync = {
          enable = true;
          server-url = "apiserver:${builtins.toString api-port}";
          username = "sync_enabled";
          password = "testpassword";
        };
      };
    };
    "scheduler_enabled" = {
      programs.smos = {
        scheduler.enable = true;
      };
    };
    "calendar_enabled" = {
      programs.smos = {
        calendar = {
          enable = true;
          sources = [
            {
              name = "Example";
              destination = "calendar.smos";
              source = "${../smos-calendar-import/test_resources/example.ics}";
            }
          ];
        };
      };
    };
    "notify_enabled" = {
      programs.smos = {
        notify.enable = true;
      };
    };
    "everything_enabled" = {
      programs.smos = {
        backup.enable = true;
        sync = {
          enable = true;
          server-url = "apiserver:${builtins.toString api-port}";
          username = "everything_enabled";
          password = "testpassword";
        };
        scheduler = {
          enable = true;
        };
        calendar = {
          enable = true;
          sources = [
            {
              name = "Example";
              destination = "calendar.smos";
              source = "${../smos-calendar-import/test_resources/example.ics}";
            }
          ];
        };
        notify = {
          enable = true;
        };
      };
    };
  };
  makeTestUser = _: _: {
    isNormalUser = true;
  };
  makeTestUserHome = username: userConfig: { pkgs, lib, ... }:
    lib.recursiveUpdate userConfig {
      imports = [
        ./home-manager-module.nix
      ];
      home.stateVersion = "20.09";
      programs.smos = {
        enable = true;
        inherit smosPackages;
      };
    };

  # The strange formatting is because of the stupid linting that nixos tests do
  commonTestScript = username: userConfig: pkgs.lib.optionalString (userConfig.programs.smos.enable or false) ''

    # Test that the config file exists.
    client.succeed(su("${username}", "cat ~/.config/smos/config.yaml"))
    # Make sure the user can run the smos commands.
    client.succeed(su("${username}", "smos --help"))
    client.succeed(su("${username}", "smos-archive --help"))
    client.succeed(su("${username}", "smos-calendar-import --help"))
    client.succeed(su("${username}", "smos-convert-org --help"))
    client.succeed(su("${username}", "smos-query --help"))
    client.succeed(su("${username}", "smos-scheduler --help"))
    client.succeed(su("${username}", "smos-sync-client --help"))
    client.succeed(su("${username}", "smos-github --help"))
    # Make sure the config file is parseable
    client.succeed(su("${username}", "smos-query next"))'';

  syncTestScript = username: userConfig: pkgs.lib.optionalString (userConfig.programs.smos.sync.enable or false) ''

    # Test that syncing works.
    client.succeed(su("${username}", "smos-sync-client register"))
    client.succeed(su("${username}", "smos-sync-client login"))
    client.succeed(su("${username}", "smos-sync-client sync"))'';

  schedulerTestScript = username: userConfig: pkgs.lib.optionalString (userConfig.programs.smos.scheduler.enable or false) ''

    # Test that the scheduler can activate.
    client.succeed(su("${username}", "smos-scheduler check"))
    client.succeed(su("${username}", "smos-scheduler schedule"))'';

  calendarTestScript = username: userConfig: pkgs.lib.optionalString (userConfig.programs.smos.calendar.enable or false) ''

    # Test that the calendar can activate.
    client.succeed(su("${username}", "smos-calendar-import"))'';

  notifyTestScript = username: userConfig: pkgs.lib.optionalString (userConfig.programs.smos.notify.enable or false) ''

    # Test that the notify can activate.
    client.succeed(su("${username}", "smos-notify"))'';

  userTestScript = username: userConfig: pkgs.lib.concatStrings [
    (commonTestScript username userConfig)
    (syncTestScript username userConfig)
    (schedulerTestScript username userConfig)
    (calendarTestScript username userConfig)
    (notifyTestScript username userConfig)
  ];

in
pkgs.nixosTest (
  { lib, pkgs, ... }: {
    name = "smos-module-test";
    nodes = {
      apiserver = {
        imports = [
          smos-production
        ];
        services.smos.production = {
          enable = true;
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
        };
      };
      webserver = {
        imports = [
          smos-production
        ];
        services.smos.production = {
          enable = true;
          web-server = {
            enable = true;
            port = web-port;
            docs-url = "docsserver:${builtins.toString docs-port}";
            api-url = "apiserver:${builtins.toString api-port}";
            web-url = "webserver:${builtins.toString web-port}";
          };
        };
      };
      docsserver = {
        imports = [
          smos-production
        ];
        services.smos.production = {
          enable = true;
          docs-site = {
            enable = true;
            port = docs-port;
            api-url = "apiserver:${builtins.toString api-port}";
            web-url = "webserver:${builtins.toString web-port}";
          };
        };
      };
      client = {
        imports = [
          home-manager
        ];
        users.users = lib.mapAttrs makeTestUser testUsers;
        home-manager = {
          useGlobalPkgs = true;
          users = lib.mapAttrs makeTestUserHome testUsers;
        };
      };
    };
    testScript = ''
      from shlex import quote

      apiserver.start()
      webserver.start()
      docsserver.start()
      client.start()
      apiserver.wait_for_unit("multi-user.target")
      webserver.wait_for_unit("multi-user.target")
      docsserver.wait_for_unit("multi-user.target")
      client.wait_for_unit("multi-user.target")

      apiserver.wait_for_open_port(${builtins.toString api-port})
      client.succeed("curl apiserver:${builtins.toString api-port}")
      webserver.wait_for_open_port(${builtins.toString web-port})
      client.succeed("curl webserver:${builtins.toString web-port}")
      docsserver.wait_for_open_port(${builtins.toString docs-port})
      client.succeed("curl docsserver:${builtins.toString docs-port}")

      # Wait for all test users
      ${lib.concatStringsSep "\n" (builtins.map (username: "client.wait_for_unit(\"home-manager-${username}.service\")") (builtins.attrNames testUsers))}
      

      def su(user, cmd):
          return f"su - {user} -c {quote(cmd)}"


      # Run the test script for each user
      ${lib.concatStrings (lib.mapAttrsToList userTestScript testUsers)}
    '';
  }
)

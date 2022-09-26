let
  sourcesFor = path:
    import (path + "/nix/sources.nix");
  pkgsFor = path: sources:
    import (path + "/nix/pkgs.nix") { inherit sources; };

in
{ name ? "smos-e2e-test"
, pathUnderTest ? ../.
, sourcesUnderTest ? sourcesFor pathUnderTest
, pkgsUnderTest ? pkgsFor pathUnderTest sourcesUnderTest
, smosReleasePackagesUnderTest ? pkgsUnderTest.smosReleasePackages
, pathOverTest ? ../.
, sourcesOverTest ? sourcesFor pathOverTest
, pkgsOverTest ? pkgsFor pathOverTest sourcesOverTest
, smosReleasePackagesOverTest ? pkgsOverTest.smosReleasePackages
}:

# This module is not symmetric.
#
# The packages under test are on the client side.
# The packages over test are on the server side.
#
# If you want to test both directions, call this tests twice with reversed arguments.
#
# See this for more info about nixos tests
# https://github.com/NixOS/nixpkgs/blob/99d379c45c793c078af4bb5d6c85459f72b1f30b/nixos/lib/testing-python.nix
let

  # Server-side configuration
  serverModule = import (pathOverTest + "/nix/nixos-module.nix") {
    sources = sourcesOverTest;
    pkgs = pkgsOverTest;
    smosReleasePackages = smosReleasePackagesOverTest;
    envname = "testing";
  };

  docs-port = 8001;
  api-port = 8002;
  web-port = 8003;


  # E2E testing configuration
  e2eTestingModule = import (pathOverTest + "/nix/end-to-end-test-nixos-module.nix") {
    sources = sourcesOverTest;
    smosReleasePackages = smosReleasePackagesOverTest;
    envname = "testing";
  };


  # Client side configuration
  home-manager = import (pkgsUnderTest.home-manager.src + "/nixos/default.nix");
  clientModule = import (pathUnderTest + "/nix/home-manager-module.nix");
  commonConfig = {
    imports = [
      clientModule
    ];
    # We must enable xdg so that :
    # * We can test that .config files are put there
    # * The ~/.config directory exist
    #   Because the systemd user services are stored in .config/systemd/user
    #   and home manager will fail to put them there otherwise.
    xdg.enable = true;
    programs.smos = {
      enable = true;
      smosReleasePackages = smosReleasePackagesUnderTest;
    };
  };

  testUsers = builtins.mapAttrs (name: config: pkgsUnderTest.lib.recursiveUpdate commonConfig config)
    {
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
  makeTestUserHome = username: userConfig: { lib, ... }: userConfig;

  # The strange formatting is because of the stupid linting that nixos tests do
  commonTestScript = username: userConfig: pkgsUnderTest.lib.optionalString (userConfig.programs.smos.enable or false) ''

    # Wait for the test user to be activated.
    client.wait_for_unit("home-manager-${username}.service")

    # Test that the config file exists.
    out = client.succeed(su("${username}", "cat ~/.config/smos/config.yaml"))
    print(out)

    # Make sure the user can run the smos commands.
    client.succeed(su("${username}", "smos --help"))
    client.succeed(su("${username}", "smos-archive --help"))
    client.succeed(su("${username}", "smos-calendar-import --help"))
    client.succeed(su("${username}", "smos-github --help"))
    client.succeed(su("${username}", "smos-query --help"))
    client.succeed(su("${username}", "smos-scheduler --help"))
    client.succeed(su("${username}", "smos-single --help"))
    client.succeed(su("${username}", "smos-sync-client --help"))

    # Make sure the config file is parseable
    client.succeed(su("${username}", "smos-query next"))'';

  backupTestScript = username: userConfig: pkgsUnderTest.lib.optionalString (userConfig.programs.smos.backup.enable or false) ''

    # Test that the local backup service and timer exist.
    client.get_unit_info("smos-backup.service", user="${username}")
    client.get_unit_info("smos-backup.timer", user="${username}")
    
    # Test that the local backup service works.
    (c, _) = client.systemctl("start --wait smos-backup.service", user="${username}")
    assert c == 0;'';

  syncTestScript = username: userConfig: pkgsUnderTest.lib.optionalString (userConfig.programs.smos.sync.enable or false) ''

    # Test that syncing works.
    client.succeed(su("${username}", "smos-sync-client register"))
    client.succeed(su("${username}", "smos-sync-client login"))
    client.succeed(su("${username}", "smos-sync-client sync"))
    client.succeed(su("${username}", "smos-single example"))
    client.succeed(su("${username}", "smos-sync-client sync"))

    # Test that the sync service and timer exist.
    client.get_unit_info("smos-sync.service", user="${username}")
    client.get_unit_info("smos-sync.timer", user="${username}")

    # Test that the sync service works.
    (c, _) = client.systemctl("start --wait smos-sync.service", user="${username}")
    assert c == 0;'';

  schedulerTestScript = username: userConfig: pkgsUnderTest.lib.optionalString (userConfig.programs.smos.scheduler.enable or false) ''

    # Test that the scheduler can activate.
    client.succeed(su("${username}", "smos-scheduler check"))
    client.succeed(su("${username}", "smos-scheduler schedule"))

    # Test that the scheduler service and timer exist.
    client.get_unit_info("smos-scheduler.service", user="${username}")
    client.get_unit_info("smos-scheduler.timer", user="${username}")

    # Test that the scheduler service works.
    (c, _) = client.systemctl("start --wait smos-scheduler.service", user="${username}")
    assert c == 0;'';

  # Tests for smos-calendar-import and its systemd service and timer
  calendarTestScript = username: userConfig: pkgsUnderTest.lib.optionalString (userConfig.programs.smos.calendar.enable or false) ''

    # Test that the calendar can activate.
    client.succeed(su("${username}", "smos-calendar-import"))

    # Test that the scheduler service and timer exist.
    client.get_unit_info("smos-calendar-import.service", user="${username}")
    client.get_unit_info("smos-calendar-import.timer", user="${username}")

    # Test that the scheduler service works.
    (c, _) = client.systemctl("start --wait smos-calendar-import.service", user="${username}")
    assert c == 0;'';

  # Tests for smos-notify and its systemd service and timer
  notifyTestScript = username: userConfig: pkgsUnderTest.lib.optionalString (userConfig.programs.smos.notify.enable or false) ''

    # Test that the notify can activate.
    client.succeed(su("${username}", "smos-notify"))

    # Test that the notify service and timer exist.
    client.get_unit_info("smos-notify.service", user="${username}")
    client.get_unit_info("smos-notify.timer", user="${username}")

    # Test that the notify service works.
    (c, _) = client.systemctl("start --wait smos-notify.service", user="${username}")
    assert c == 0;'';

  userTestScript = username: userConfig: pkgsUnderTest.lib.concatStrings [
    (commonTestScript username userConfig)
    (backupTestScript username userConfig)
    (syncTestScript username userConfig)
    (schedulerTestScript username userConfig)
    (calendarTestScript username userConfig)
    (notifyTestScript username userConfig)
  ];

in
pkgsUnderTest.nixosTest (
  { lib, ... }: {
    inherit name;
    nodes = {
      apiserver = {
        imports = [
          serverModule
        ];
        services.smos.testing = {
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
          serverModule
        ];
        services.smos.testing = {
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
          serverModule
        ];
        services.smos.testing = {
          enable = true;
          docs-site = {
            enable = true;
            port = docs-port;
            api-url = "apiserver:${builtins.toString api-port}";
            web-url = "webserver:${builtins.toString web-port}";
          };
        };
      };
      client = { config, ... }: {
        imports = [
          home-manager
        ];
        users.users = lib.mapAttrs makeTestUser testUsers;
        system.activationScripts = {
          # We must enable lingering so that the Systemd User D-Bus is enabled.
          # We also cannot do this with loginctl enable-linger because it needs to happen before systemd is loaded.
          # It would be nice if there were a nixos option for this.
          # See https://github.com/NixOS/nixpkgs/issues/3702
          enableLingering =
            let touchUserLinger = username: _: "touch /var/lib/systemd/linger/${username}";
            in
            ''
              # remove all existing lingering users
              rm -rf /var/lib/systemd/linger
              mkdir -p /var/lib/systemd/linger
              # enable for the subset of declared users
              ${lib.concatStringsSep "\n" (lib.mapAttrsToList touchUserLinger config.users.users)}
            '';
        };

        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          users = lib.mapAttrs makeTestUserHome testUsers;
        };
      };
      e2etestclient = {
        imports = [
          e2eTestingModule
        ];
        services.smos.testing.end-to-end-testing = {
          enable = true;
          api-server = {
            enable = true;
            tests = {
              testing = {
                enable = true;
                api-url = "apiserver:${builtins.toString api-port}";
                time = "00:00";
              };
            };
          };
        };
      };
    };
    testScript = ''
      from shlex import quote

      apiserver.start()
      webserver.start()
      docsserver.start()
      client.start()
      e2etestclient.start()
      apiserver.wait_for_unit("multi-user.target")
      webserver.wait_for_unit("multi-user.target")
      docsserver.wait_for_unit("multi-user.target")
      client.wait_for_unit("multi-user.target")
      e2etestclient.wait_for_unit("multi-user.target")

      print("starting end-to-end-tests")
      client.systemctl("start smos-api-server-end-to-end-test-testing-testing.timer")
      client.systemctl("start smos-api-server-end-to-end-test-testing-testing.service --wait")
      print("end-to-end-tests done")


      apiserver.wait_for_open_port(${builtins.toString api-port})
      client.succeed("curl apiserver:${builtins.toString api-port}")
      webserver.wait_for_open_port(${builtins.toString web-port})
      client.succeed("curl webserver:${builtins.toString web-port}")
      docsserver.wait_for_open_port(${builtins.toString docs-port})
      client.succeed("curl docsserver:${builtins.toString docs-port}")
      

      def su(user, cmd):
          return f"su - {user} -c {quote(cmd)}"


      # Run the test script for each user
      ${lib.concatStrings (lib.mapAttrsToList userTestScript testUsers)}
    '';
  }
)

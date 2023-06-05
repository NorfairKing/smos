{ nixosTest
, system
, get-flake
, home-manager
}:
{ name
, flakeUnderTest
, flakeOverTest
}:
# This module is not symmetric.
#
# The packages under test are on the client side.
# The packages over test are on the server side.
#
# If you want to test both directions, call this tests twice with reversed arguments.
nixosTest ({ lib, pkgs, ... }:
with lib;
let
  # Server-side configuration
  serverModule = flakeOverTest.nixosModules.${system}.default;

  docs-port = 8001;
  api-port = 8002;
  web-port = 8003;

  # E2E testing configuration
  e2eTestingModule = flakeOverTest.nixosModules.${system}.e2eTest;

  # Client side configuration
  clientModule = flakeUnderTest.homeManagerModules.${system}.default;
  commonClientConfig = {
    imports = [ clientModule ];
    home.stateVersion = "23.05";
    # We must enable xdg so that:
    # * We can test that .config files are put there
    # * The ~/.config directory exist
    #   Because the systemd user services are stored in .config/systemd/user
    #   and home manager will fail to put them there otherwise.
    xdg.enable = true;
    programs.smos = {
      enable = true;
    };
    systemd.user.startServices = "sd-switch";
  };

  testUsers = builtins.mapAttrs (name: config: recursiveUpdate commonClientConfig config) {
    "nothing_enabled" = { };
    "backup_enabled" = {
      programs.smos.backup.enable = true;
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
      programs.smos.scheduler.enable = true;
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
      programs.smos.notify.enable = true;
    };
    "github_enabled" = {
      programs.smos.github.enable = true;
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
        scheduler.enable = true;
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
        notify.enable = true;
        github.enable = true;
      };
    };
  };
  makeTestUser = _: _: {
    isNormalUser = true;
  };
  makeTestUserHome = username: userConfig: { lib, ... }: userConfig;

  # The strange formatting is because of the stupid linting that nixos tests do
  commonTestScript = username: userConfig: optionalString (userConfig.programs.smos.enable or false) ''

    # Show the user units
    (_, statusout_${username}) = client.systemctl("list-units 'smos-*'", user="${username}")
    print(statusout_${username})

    # Wait for the test user to be activated.
    client.wait_for_unit("home-manager-${username}.service")

    # Test that the config file exists.
    config_${username} = client.succeed(su("${username}", "cat ~/.config/smos/config.yaml"))
    print(config_${username})

    # Make sure the user can run the smos commands.
    client.succeed(su("${username}", "smos --help"))
    client.succeed(su("${username}", "smos-archive --help"))
    client.succeed(su("${username}", "smos-jobhunt --help"))
    client.succeed(su("${username}", "smos-query --help"))
    client.succeed(su("${username}", "smos-single --help"))

    # Make sure the config file is parseable
    client.succeed(su("${username}", "smos-query next"))'';

  backupTestScript = username: userConfig: optionalString (userConfig.programs.smos.backup.enable or false) ''

    # Test that the local backup service and timer exist.
    client.get_unit_info("smos-backup.service", user="${username}")
    client.get_unit_info("smos-backup.timer", user="${username}")
    
    # Test that the local backup service works.
    backup_status_${username} = client.systemctl("start --wait smos-backup.service", user="${username}")[0]
    assert backup_status_${username} == 0'';

  syncTestScript = username: userConfig: optionalString (userConfig.programs.smos.sync.enable or false) ''

    # Test that the sync client is installed
    client.succeed(su("${username}", "smos-sync-client --help"))

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
    sync_status_${username} = client.systemctl("start --wait smos-sync.service", user="${username}")[0]
    assert sync_status_${username} == 0'';

  schedulerTestScript = username: userConfig: optionalString (userConfig.programs.smos.scheduler.enable or false) ''

    # Test that the scheduler is installed
    client.succeed(su("${username}", "smos-scheduler --help"))

    # Test that the scheduler can activate.
    client.succeed(su("${username}", "smos-scheduler check"))
    client.succeed(su("${username}", "smos-scheduler schedule"))

    # Test that the scheduler service and timer exist.
    client.get_unit_info("smos-scheduler.service", user="${username}")
    client.get_unit_info("smos-scheduler.timer", user="${username}")

    # Test that the scheduler service works.
    scheduler_status_${username} = client.systemctl("start --wait smos-scheduler.service", user="${username}")[0]
    assert scheduler_status_${username} == 0'';

  # Tests for smos-calendar-import and its systemd service and timer
  calendarTestScript = username: userConfig: optionalString (userConfig.programs.smos.calendar.enable or false) ''

    # Test that the calendar can activate.
    client.succeed(su("${username}", "smos-calendar-import --help"))

    # Test that the scheduler service and timer exist.
    client.get_unit_info("smos-calendar-import.service", user="${username}")
    client.get_unit_info("smos-calendar-import.timer", user="${username}")

    # Test that the scheduler service works.
    calendar_status_${username} = client.systemctl("start --wait smos-calendar-import.service", user="${username}")[0]
    assert calendar_status_${username} == 0'';

  # Tests for smos-notify and its systemd service and timer
  notifyTestScript = username: userConfig: optionalString (userConfig.programs.smos.notify.enable or false) ''

    # Test that notify is installed
    client.succeed(su("${username}", "smos-notify --help"))

    # Test that notify can activate.
    # client.succeed(su("${username}", "smos-notify"))
    # FIXME: Figure out why this started erroring with
    # Failed to connect to bus: No such file or directory

    # Test that the notify service and timer exist.
    client.get_unit_info("smos-notify.service", user="${username}")
    client.get_unit_info("smos-notify.timer", user="${username}")

    # Test that the notify service works.
    notify_status_${username} = client.systemctl("start --wait smos-notify.service", user="${username}")[0]
    assert notify_status_${username} == 0'';

  # Tests for smos-github
  githubTestScript = username: userConfig: optionalString (userConfig.programs.smos.github.enable or false) ''

    # Test that smos-github is installed.
    client.succeed(su("${username}", "smos-github --help"))'';

  userTestScript = username: userConfig: concatStrings [
    (commonTestScript username userConfig)
    (backupTestScript username userConfig)
    (syncTestScript username userConfig)
    (schedulerTestScript username userConfig)
    (calendarTestScript username userConfig)
    (notifyTestScript username userConfig)
    (githubTestScript username userConfig)
  ];

in
{
  inherit name;
  nodes = {
    apiserver = {
      imports = [
        serverModule
      ];
      system.stateVersion = "23.05";
      time.timeZone = "Europe/Zurich";
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
        serverModule
      ];
      system.stateVersion = "23.05";
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
        serverModule
      ];
      system.stateVersion = "23.05";
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
    client = { config, ... }: {
      imports = [
        home-manager
      ];
      users.users = mapAttrs makeTestUser testUsers;
      system.stateVersion = "23.05";
      # We must enable lingering so that the Systemd User D-Bus is enabled.
      # We also cannot do this with loginctl enable-linger because it needs to happen before systemd is loaded.
      # It would be nice if there were a nixos option for this.
      # See https://github.com/NixOS/nixpkgs/issues/3702
      system.activationScripts = {
        enableLingering =
          let touchUserLinger = username: _: "touch /var/lib/systemd/linger/${username}";
          in
          ''
            # remove all existing lingering users
            rm -rf /var/lib/systemd/linger
            mkdir -p /var/lib/systemd/linger
            # enable for the subset of declared users
            ${lib.concatStringsSep "\n" (lib.mapAttrsToList touchUserLinger testUsers)}
          '';
      };

      home-manager = {
        useGlobalPkgs = true;
        useUserPackages = true;
        verbose = true;
        users = mapAttrs makeTestUserHome testUsers;
      };
    };
    e2etestclient = {
      imports = [
        e2eTestingModule
      ];
      system.stateVersion = "23.05";
      services.smos.production.end-to-end-testing = {
        enable = true;
        api-server = {
          enable = true;
          tests = {
            production = {
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
    apiserver.wait_for_unit("default.target")
    webserver.wait_for_unit("default.target")
    docsserver.wait_for_unit("default.target")
    client.wait_for_unit("default.target")
    e2etestclient.wait_for_unit("default.target")

    print("starting end-to-end-tests")
    client.systemctl("start smos-api-server-end-to-end-test-production-production.timer")
    client.systemctl("start smos-api-server-end-to-end-test-production-production.service --wait")
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
    ${concatStrings (mapAttrsToList userTestScript testUsers)}
  '';

})

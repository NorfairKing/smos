# Smos

![Stack Build](https://github.com/NorfairKing/smos/workflows/Stack%20Build/badge.svg)
![Nix Build](https://github.com/NorfairKing/smos/workflows/Nix%20Build/badge.svg)

See [the documentation site](https://docs.smos.online) for more information.

<img src="smos-docs-site/content/assets/logo.svg" width="200" alt="logo"/>

## Try it out!

There is a demo playground at `smos-docs-site/demo-workflow`.
You can run `nix-shell` there and try out different things.

## Hacking on Smos

### Package overview

```
smos-data                : The Smos data format
smos-data-gen            : Generators for the Smos data format
smos-cursor              : Smos-specific cursors
smos-cursor-gen          : Generators for Smos-specific cursors
smos-report              : Smos Reports
smos-report-gen          : Generators for Smos reports
smos-report-cursor       : Cursors for Smos reports
smos-report-cursor-gen   : Generators for cursors for smos reports
smos                     : The smos TUI
smos-single              ; The smos-single tool; to make a single-task project
smos-query               : The smos-query tool; to query your workflow
smos-archive             : The smos-archive tool: to archive smos files
smos-convert-org         : The smos-convert-org tool: to convert org-mode files to smos files
smos-calendar-import     : The calendar import tool: to import calendars in an onging manner
smos-api                 : The API for the smos server
smos-api-gen             : Generators for the API for the smos server
smos-client              : The smos client library
smos-client-gen          : Generators for the smos client library
smos-server              : The smos server
smos-server-gen          : Generators for the smos server
smos-sync-client         : The sync client tool
smos-web-style           : The stylesheets for the web server and docs site
smos-web-server          : The smos web server
smos-docs-site           : The generator for the documentation website.
```

### Development

Before starting development, make sure to run `nix-shell` at least once to install pre-commit hooks.

Before submitting a PR, make sure that these commands _all_ succeed:

```
nix-build ci.nix
stack clean
stack build --test --bench --no-run-benchmarks --pedantic
```

## Cachix cache

There is a [cachix](https://cachix.org) cache for this project.

To use it, use `cachix use smos` or add the appropriate details to your nixos configuration.


## Sponsor this project

Smos is free and open-source software.
Building it takes time, energy and money.
Please consider supporting the project at https://docs.smos.online/support


{ lib, ... }:
# Set the pedantic build up with https://github.com/input-output-hk/haskell.nix/issues/519 when that works.
{
  packages =
    # Set extra flags (see stack.yaml) until https://github.com/input-output-hk/haskell.nix/issues/827 is fixed.
    let
      ps = [
        "smos"
        "smos-data"
        "smos-data-gen"
        "smos-cursor"
        "smos-cursor-gen"
        "smos-report"
        "smos-report-gen"
        "smos-report-cursor"
        "smos-report-cursor-gen"
        "smos-query"
        "smos-single"
        "smos-scheduler"
        "smos-archive"
        "smos-convert-org"
        "smos-calendar-import"
        "smos-api"
        "smos-api-gen"
        "smos-server"
        "smos-server-gen"
        "smos-client"
        "smos-client-gen"
        "smos-sync-client"
        "smos-sync-client-gen"
        "smos-web-server"
        "smos-docs-site"
      ];
      extraFlags = "-Werror -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Wpartial-fields -Widentities -Wredundant-constraints -Wcpp-undef -Wcompat";
    in
      lib.genAttrs ps (p: { package.ghcOptions = extraFlags; });
}

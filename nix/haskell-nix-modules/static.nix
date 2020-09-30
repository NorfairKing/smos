{ pkgs, lib, ... }:
let
  exes = [
    "smos"
    "smos-archive"
    "smos-asciinema"
    "smos-calendar-import"
    "smos-convert-org"
    "smos-docs-site"
    "smos-query"
    "smos-scheduler"
    "smos-server"
    "smos-single"
    "smos-sync-client"
    "smos-web-server"
  ];
in
{
  packages = lib.optionalAttrs pkgs.stdenv.hostPlatform.isMusl (
    lib.genAttrs exes (
      exe:
        {
          enableStatic = true;
          components.exes."${exe}" = {
            enableStatic = true;
            dontStrip = false;
            enableShared = false;
            configureFlags = [
              "--disable-executable-dynamic"
              "--disable-shared"
              # "--ghc-option=-optl=-pthread"
              "--ghc-option=-optl=-static"
              # "--extra-lib-dirs=${final.numactl}/lib"
              "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
              "--extra-lib-dirs=${pkgs.zlib}/lib"
              "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
              "--extra-lib-dirs=${pkgs.ncurses.override { enableStatic = true; }}/lib"
            ];
          };
        }
    )
  );
}

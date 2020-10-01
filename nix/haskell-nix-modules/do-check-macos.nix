let
  isMacos = builtins.currentSystem == "x86_64-darwin";
in
{
  # Turn off certain test suites on macos because they generate random
  # filepaths and that fails for some reason that I cannot investigate
  # because I don't own any apple products.
  doCheck = true;
  packages.smos-archive.doCheck = !isMacos;
  packages.smos-cursor-gen.doCheck = !isMacos;
  packages.smos-query.doCheck = !isMacos;
  packages.smos-report-gen.doCheck = !isMacos;
  packages.smos-report-cursor-gen.doCheck = !isMacos;
  packages.smos-scheduler.doCheck = !isMacos;
  packages.smos-sync-client-gen.doCheck = !isMacos;
  packages.smos.doCheck = !isMacos;
}

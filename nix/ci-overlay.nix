final: previous:
let
  ciVersionInfo = final.writeText "ci-version-info" ''
    This is a CI build without version information. You should not be using this in practice because then you cannot submit good bug reports.
  '';
in
{
  smosVersionInfo = ciVersionInfo;
}

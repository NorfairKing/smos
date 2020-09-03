final: previous:
let
  ciVersionInfo = final.writeText "ci-version-info" ''
    This is a CI build without version information
  '';
in
{
  smosVersionInfo = ciVersionInfo;
}

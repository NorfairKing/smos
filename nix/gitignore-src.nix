final: previous:
let
  gitignoreSrc =
    builtins.fetchGit {
      url = "https://github.com/hercules-ci/gitignore.nix";
      ref = "master";
      rev = "c4662e662462e7bf3c2a968483478a665d00e717";
    };
in
{
  inherit (import gitignoreSrc { inherit (final) lib; }) gitignoreSource gitignoreFilter;
}

final: previous:
let
  gitignoreSrc =
    final.fetchFromGitHub {
      owner = "hercules-ci";
      repo = "gitignore.nix";
      rev = "c4662e662462e7bf3c2a968483478a665d00e717";
      sha256 = "sha256:1npnx0h6bd0d7ql93ka7azhj40zgjp815fw2r6smg8ch9p7mzdlx";
    };
in
{
  inherit (import gitignoreSrc { inherit (final) lib; }) gitignoreSource;
}

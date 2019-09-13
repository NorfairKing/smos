final: previous:
let
  gitignoreSrc =
    final.fetchFromGitHub {
      owner = "hercules-ci";
      repo = "gitignore";
      rev = "ec4a0039152655b6c919d289dafd7ba32206ea1f";
      sha256 = "sha256:13qxqbs8jg2mz2fm2cs63czv30gxi39ws5qzf9j8mczqpdj6g3im";
    };
in {
  inherit (import gitignoreSrc { inherit (final) lib; }) gitignoreSource;
}

let
  pkgs = import ./nix/pkgs.nix;
  nix-pre-commit-hooks =
    import (
      builtins.fetchTarball "https://github.com/hercules-ci/nix-pre-commit-hooks/archive/f709c4652d4696dbe7c6a8354ebd5938f2bf807b.tar.gz"
    );
in
pkgs.smosPackages // {
  "smos-docs-site" = pkgs.smosDocumentationSite;
  pre-commit-check = nix-pre-commit-hooks.run {
    src = ./.;
    hooks = {
      nixpkgs-fmt.enable = true;
      hlint.enable = true;
      ormolu.enable = true;
    };
  };
}

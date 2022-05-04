---
title: Documentation Site
description: How to contribute to the Smos Documentation Site
---

## Setting up a feedback loop

To contribute to the documentation site, use `feedback`:

```
$ nix-shell
nix-shell $ feedback docs
```

## Contributing casts

Contributing an screencast to the documentation site involves writing an `autorecorder` cast specification.
This ensures that the casts are always showing the current version of the tools that they show off.

1. Install `autorecorder` [from source](https://github.com/NorfairKing/autorecorder)
2. Locate the cast spec files in `smos-docs-site/content/casts`.
3. Copy the example cast: `example.yaml` to create your new `mycast.yaml` specification.
4. Make the changes that you want.
5. Try out your cast using `autorecorder record mycast.yaml mycast.cast`.
6. Use your cast in the documentation site by embedding it like this:

   ``` html
     <asciinema-player
       src="/casts/mycast.cast"
       autoplay="true"
       preloop="true"
       loop="true">
      </asciinema-player>
   ```

## Contributing to the NixOS Module Docs

Building [the NixOS module docs](/nix/nixos-module) requires a nix build, so they are not built in the default feedback loop for `smos-docs-site`.
If you want to work on them from the `stack` feedback loop anyway, you can build the module docs with `nix-build nix/pkgs.nix -A moduleDocs` and then set `MODULE_DOCS=result/share/doc/nixos/options.json` before running the feedback loop:

```
MODULE_DOCS=../result/share/doc/nixos/options.json ./scripts/devel.sh  
```

Note that you may have to `stack clean smos-docs-site` to get the build to pick up this change.

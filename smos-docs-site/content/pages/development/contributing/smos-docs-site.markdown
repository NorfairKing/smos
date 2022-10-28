---
title: Documentation Site
description: How to contribute to the Smos Documentation Site
---

## Setting up a feedback loop

To contribute to the documentation site, use `feedback`:

```
$ nix develop
nix $ feedback docs
```

## Contributing casts

Contributing an screencast to the documentation site involves writing an `autorecorder` cast specification.
This ensures that the casts are always showing the current version of the tools that they show off.

1. Locate the cast spec files in `smos-docs-site/content/casts`.
1. Copy the example cast: `example.yaml` to create your new `mycast.yaml` specification.
1. Make the changes that you want.
1. Try out your cast using `autorecorder record mycast.yaml mycast.cast`.
1. Use your cast in the documentation site by embedding it like this:

   ``` html
     <asciinema-player
       src="/casts/mycast.cast"
       autoplay="true"
       preloop="true"
       loop="true">
      </asciinema-player>
   ```

## Contributing to the NixOS Module Docs

Building [the NixOS module docs](/nix/nixos-module) or [the Nix home manager module docs](/nix/home-manager-module) require a nix build, so they are not built in the default feedback loop for `smos-docs-site`.
If you want to work on them from the `stack` feedback loop anyway, you can build the module docs with these commands before running the feedback loop:

* `nix build .#nixosModuleDocs` and then set `NIXOS_MODULE_DOCS=result/share/doc/nixos/options.json`
* `nix build .#homeManagerModuleDocs` and then set `HOME_MANAGER_MODULE_DOCS=result/share/doc/nixos/options.json`

```
feedback docs
```

Note that you may have to `stack clean smos-docs-site` to get the build to pick up this change.

---
title: Building with stack
description: Documentation about installing Smos using stack
---

It is possible to build smos using [Stack](https://haskellstack.org), but this method is mainly available for development purposes.
In practice we recommend using Nix for installing Smos.


1. Clone the repository:

   ```
   git clone https://github.com/NorfairKing/smos
   ```

1. Decide whether you want to build the `smos-server` executable.
   If you just want to use `smos`, `smos-query` and the other command-line executables, the answer will be "no".
   In that case, you can remove the `stripe-client` line from the `stack.yaml` file and continue to the next step.

   If you do want to build `smos-server`, then you need to generate the `stripe-client` package.

   1. If you have nix installed, you can run this command and skip the rest of this step:

      ```
      ./scripts/generate-stripe.sh
      ```

   1. Get the stripe OpenAPI spec.
      The `nix/sources.json` file specifies the version of `stripe-spec` to use.
      You will need the `openapi/spec3.yaml` file from that repository.

   1. Get the openapi3 client code generator:

      ```
      stack install openapi3-code-generator --stack-yaml setup-stack.yaml
      ```

   1. Generate the `stripe-client` library using this command:

      ```
      ~/.local/bin/openapi3-code-generator-exe --specification /path/to/openapi/spec3.yaml --configuration stripe-client-gen.yaml
      ```


1. Install prerequisites:

   ```
   stack install autoexporter
   ```

1. Install Smos with the default configuration:

   ```
   stack install smos
   ```

1. You probably want to also install related tools:

   ```
   stack install smos-query
   stack install smos-archive
   stack install smos-single
   stack install smos-sync-client
   ```

### Troubleshooting

#### Stripe client

```
/home/user/src/smos/stripe-client/: getDirectoryContents:openDirStream: does not exist (No such file or directory)
```

If you see the above error, you probably skipped the "Generate the stripe client" steps above.
This happens because we generate the stripe client based on the openapi3 spec for Stripe's api.
This generated code is not committed to the repository.

#### Autoconf

You may get an error about `autoreconf` not being available if `autoconf` is not installed.
Make sure that you have `autoreconf` on your path.

#### Arch linux

*Do not install `stack` or any other haskell packages using pacman.*
This is unlikely to build at all and will most likely cause in bugs if it does.
Instead, install `stack` by following the documentation [on this page](https://docs.haskellstack.org/en/stable/README/#how-to-install)

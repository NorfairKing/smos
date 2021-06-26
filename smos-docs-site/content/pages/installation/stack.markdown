---
title: Building with stack
description: Documentation about installing Smos using stack
---

To build Smos using [Stack](https://haskellstack.org), first clone the repository:

```
git clone https://github.com/NorfairKing/smos
```

Install prerequisites:

```
# Compile the openapi client library generator
stack install openapi3-code-generator --stack-yaml setup-stack.yaml
# Generate the stripe client
./scripts/generate-stripe-api.sh
# Compile tool
stack install autoexporter
```

Now you can install Smos with the default configuration:

```
stack install smos
```

You probably want to also install related tools:

```
stack install smos-query
stack install smos-archive
stack install smos-single
stack install smos-sync-client
```

If you just want to make sure to get everything:

```
stack install
```

### Troubleshooting

#### Stripe client

```
/home/user/src/smos/stripe-api/stripe-client/: getDirectoryContents:openDirStream: does not exist (No such file or directory)
```'s api.
This generated code is not committed to the repository.

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

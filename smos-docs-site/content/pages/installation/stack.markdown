---
title: Building with stack
---

To build Smos using [Stack](https://haskellstack.org), first clone the repository:

```
git clone https://github.com/NorfairKing/smos
```

Install prerequisites:

```
stack install autoexporter # Only for compilation
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

#### Troubleshooting on Arch linux

*Do not install `stack` or any other haskell packages using pacman.*
This is unlikely to build at all and will most likely cause in bugs if it does.
Instead, install `stack` by following the documentation [on this page](https://docs.haskellstack.org/en/stable/README/#how-to-install)

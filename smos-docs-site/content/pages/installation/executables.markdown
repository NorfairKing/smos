---
title: Downloading executables
description: Documentation about using smos without any building via the statically-linked executables
---

### Statically linked executables

Each release contains a `release.zip` archive with statically linked executables.
You can download it from [the "Assets" section on github](https://github.com/NorfairKing/smos/releases/latest).


These binaries are entirely statically linked (against `musl`, not `glibc`) so you can just go ahead and use them as-is:

```
$ ldd smos
	not a dynamic executable
```

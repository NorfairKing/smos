---
title: Installation on Windows
description: Documentation about using Smos from Windows
---

Installing smos on windows is not as straight-forward as we would like it to be. (See [Issue 52](https://github.com/NorfairKing/smos/issues/52))
Windows users may want to [use Smos online](https://smos.online) instead.
However, it is possible to use Smos on windows via the Windows Subsystem for Linux (WSL).

### Installing Smos on the WSL

1. [Enable the WSL](https://docs.microsoft.com/en-us/windows/wsl/install-win10)
2. Enable virtualisation in your bios
3. [Install Ubuntu on WSL](https://ubuntu.com/wsl)
4. Update the local package database: `sudo apt-get update`.
5. Install `libtinfo`: `sudo apt-get install libtinfo-dev`.
6. Follow [the stack installation instructions](/installation/stack).

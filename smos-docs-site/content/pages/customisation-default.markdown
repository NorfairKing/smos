---
title: Customisation
description: Documentation about the different ways to customise your usage of Smos
---

Smos can be configured very flexibly, via flags, environment variables and configuration files.
For a complete overview, you see the [reference documentation](/smos).

For example, to configure a different workflow directory than the default `~/workflow`, you can use
the command-line flag `--workflow-dir` or the environment variable `SMOS_WORKFLOW_DIR`, or
you can use a configuration file.

The default configuration files are (in the order that they are tried):

- `$XDG_CONFIG_HOME/smos/config.yaml`
- `$HOME/.smos/config.yaml`
- `$HOME/.smos.yaml`

For example, to change the workflow directory, you can this YAML config:

``` yaml
workflow-dir: "/home/user/different-workflow"
```


You can also reconfigure the keybindings using, for example:

``` yaml
workflow-dir: "/home/user/different-workflow"
keys:
  reset: false
  file:
    entry:
      - action: subtreeUnsetTodoState
        key: T<space>
      - action: forestToggleCollapseRecursively
        key: <tab>
      - action: forestToggleCollapse
        key: M-<tab>
    header:
      - action: headerMoveToStart
        key: <PageUp>
      - action: headerMoveToEnd
        key: <PageDown>
```


Smos can also be configured directly in Haskell.
If you are looking to do that, please see [the respective guide](/customisation-haskell)

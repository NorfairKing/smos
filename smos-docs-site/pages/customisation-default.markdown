---
title: Customisation
date: 2018-11-13
---

Smos can be configured very flexibly, via flags, environment variables and configuration files.

For example, to configure a different workflow directory than the default `~/workflow`, you can use
the command-line flag `--workflow-dir` or the environment variable `SMOS_WORKFLOW_DIR`, or
you can use a configuration file.

The default configuration files are (in the order that they are tried):

- `~/.smos.yaml` in [YAML](http://yaml.org/)
- `~/.smos.json` in [JSON](http://json.org/)

For example, to change the workflow directory, you can use:

- This YAML config:

``` yaml
workflow-dir: "/home/user/different-workflow"
```

- This JSON config:

``` json
{ "workflow-dir": "/home/user/different-workflow" }
```

You can also reconfigure the keybindings using, for example ...

- This YAML config:

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
        key: PageUp
      - action: headerMoveToEnd
        key: PageDown
```

See [the golden test](https://github.com/NorfairKing/smos/blob/master/smos/test_resources/config/yaml/workflow.yaml) for a more comprehensive example of all the possible options.

- This JSON config:

``` json
{ "workflow-dir": "/home/user/different-workflow",
  "keys": {
    "reset": false,
    "file": {
      "entry": [
        { "action": "subtreeUnsetTodoState",
          "key": "T<space>"
        },
        { "action": "forestToggleCollapseRecursively",
          "key": "<tab>"
        },
        { "action": "forestToggleCollapse",
          "key": "M-<tab>"
        }
      ],
      "header": [
        { "action": "headerMoveToStart",
          "key": "PageUp"
        },
        { "action": "headerMoveToEnd",
          "key": "PageDown"
        }
      ]
    }
  }
}
```

See [the golden test](https://github.com/NorfairKing/smos/blob/master/smos/test_resources/config/json/workflow.json) for a more comprehensive example of all the possible options.

Smos can also be configured directly in Haskell.
If you are looking to do that, please see [the respective guide](/customisation-haskell.html)

---
title: Customisation
date: 2018-11-13
---

Smos can be configured very flexibly, via flags, environment variables and configuration files.

For example, to configur a different workflow directory than the default `~/workflow`, you can use
the command-line flag `--workflow-dir` or the environment variable `SMOS_WORKFLOW_DIR`, or
you can use a configuration file.

The default configuration files are (in the order that they are tried):

- `~/.smos.dhall`: in [Dhall](https://github.com/dhall-lang/dhall-lang)
- `~/.smos.yaml` in [YAML](http://yaml.org/)
- `~/.smos.json` in [JSON](http://json.org/)

For example, to change the workflow directory, you can use:

- This Dhall config:

``` dhall
{ workflow-dir = [ "/home/user/different-workflow" ] : Optional Text
}
```

- This YAML config:

``` yaml
workflow-dir: "/home/user/different-workflow"
```

- This JSON config:

``` json
{ "workflow-dir": "/home/user/different-workflow"
}
```

Smos can also be configured more flexibly in Haskell.
If you are looking to do that, please see [the respective guide](/pages/customisation-haskell.html)

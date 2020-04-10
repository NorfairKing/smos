---
title: Customisation Reference
date: 2020-04-07
---

# Misc #

contexts

: ???

work-filter

: ???

# Customizing directories #

archive-dir

: the archive directory

workflow-dir

: the workflow directory

projects-dir

: the projects directory

archived-projects-dir

: the archive projects directory


# Customising keybindings #

<!-- NOTE: It should be nice to be able to generate the reference for
           the keybingings automatically, since the information are
           already present in the help screen -->

Keybindings in smos are context sensitive.

The generic syntax for keybindings is:

```yaml
keys:
    context:
        subcontext:
        - action: actionName
          key: <Esc>
```

The following paragraph lists the valid contexts and the valid
keybindings.

## reports ##

In the `reports` context there is only one subcontext: `next-action`.

### reports/next-action ###

## help ##

In the `help` context there are two subcontexts: `help` and `search`.

### help/help ###

### help/search ###

## file ##

### file/empty ###

### file/logbook ###

### file/entry ###

### file/header ###

### file/timestamps ###

### file/state-history ###

### file/any ###

### file/tags ###

### file/properties ###


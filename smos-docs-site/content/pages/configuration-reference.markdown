---
title: Customisation Reference
date: 2020-04-07
---

## Misc ##

contexts

: ???

work-filter

: ???

## Customizing directories ##

archive-dir

: the archive directory

workflow-dir

: the workflow directory

projects-dir

: the projects directory

archived-projects-dir

: the archive projects directory


## Defining reports ##

<!-- TODO:  -->

## Customising keybindings ##

Keybindings in smos are context sensitive.

The generic syntax for keybindings is:

```yaml
keys:
    context:
        subcontext:
        - action: actionName
          key: <Esc>
```

The syntax for *special* keys follows (the meaning of each should be obvious):

- \<tab\>
- \<space>
- \<UpRight>
- \<UpLeft>
- \<Up>
- \<Right>
- \<PrtScr>
- \<Pause>
- \<PageUp>
- \<PageDown>
- \<Menu>
- \<Left>
- \<Ins>
- \<Home>
- \<Esc>
- \<Enter>
- \<End>
- \<DownRight>
- \<DownLeft>
- \<Down>
- \<Del>
- \<Center>
- \<Begin>
- \<BackTab>
- \<BS>
- \<F0> .. \<F9>

The full list of available commands can be found in [the Command
Reference](/commands.html).

The following paragraph lists the valid contexts and the valid
keybindings.

### reports ###

In the `reports` context there is only one subcontext: `next-action`.

#### reports/next-action ####

### help ###

In the `help` context there are two subcontexts: `help` and `search`.

#### help/help ####

#### help/search ####

### file ###

#### file/empty ####

#### file/logbook ####

#### file/entry ####

#### file/header ####

#### file/timestamps ####

#### file/state-history ####

#### file/any ####

#### file/tags ####

#### file/properties ####

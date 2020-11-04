---
title: Customisation Reference
description: Reference documentation for the customisation
---

Keybindings in smos are context sensitive.

The generic syntax for keybindings is:

```yaml
keys:
    context:
        subcontext:
        - action: actionName
          key: <Esc>
```

Valid values for `context` and `subcontext` are:

- `file`
  - `empty`
  - `header`
  - `contents`
  - `tags`
  - `state-history`
  - `timestamps`
  - `entry`
  - `logbook`
  - `any`
- `help`
  - `help`
  - `search`
- `reports`
  - `next-action`

Under `key` you can list any number of characters without spaces:

```
- action: helpStart
  key: gg
```

You can also use modifier keys like so:

```
- action: selectHelp
  key: M-?
```

The syntax for *special* keys follows:

- `<tab>`
- `<space>`
- `<UpRight>`
- `<UpLeft>`
- `<Up>`
- `<Right>`
- `<PrtScr>`
- `<Pause>`
- `<PageUp>`
- `<PageDown>`
- `<Menu>`
- `<Left>`
- `<Ins>`
- `<Home>`
- `<Esc>`
- `<Enter>`
- `<End>`
- `<DownRight>`
- `<DownLeft>`
- `<Down>`
- `<Del>`
- `<Center>`
- `<Begin>`
- `<BackTab>`
- `<BS>`
- `<F1>` .. `<F12>`

TODO full list of available actions

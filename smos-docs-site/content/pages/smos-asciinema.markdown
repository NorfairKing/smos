---
title: The Smos ASCIInema Automation Tool
---

This tool exists to automate smos casts for documentation purposes.
You can use it to produce a cast this:

<asciinema-player
  src="/casts/example.cast"
  autoplay="true"
  preloop="true"
  loop="true">
  </asciinema-player>

from a spec like this:

```
command: smos example.smos
file: example.smos
input:
- send: e
- wait: 1000
- type: Hello world!
- wait: 1000
- send: "\e"
- wait: 1000
- send: 'q'
- wait: 1000
```

See [the examples](https://github.com/NorfairKing/smos/tree/master/smos-asciinema/examples) in the source code,
as well as [the source code for the casts on this site](https://github.com/NorfairKing/smos/tree/master/smos-docs-site/content/casts).

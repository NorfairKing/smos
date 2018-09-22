# Smos

A semantic tree-based editor to replace Emacs Org Mode

## A replacement for emacs org mode

Smos intends to deprecate Emacs org mode by being more robust, more
customisable, by providing a better and more tool-friendly file
format and by using a more sane configuration language.

## Completely customisable

Smos is first and foremost a library with which you can build your own version.
It is similar to XMonad in this respect.
See [the default configuration](smos/src/Smos/Default.hs)
for an example of a configuration.

## Future-proof file format

The Smos file format (`.smos`) is completely future proof because it is just
a subset of YAML:

![Example](assets/smos.png)

```
- entry:
    header: Use Smos
    state-history:
    - - TODO
      - 2017-11-06T23:32:58.574724188Z
  forest:
  - header: Don't mess it up
    state-history:
    - - DONE
      - 2017-11-06T23:32:59.17487692Z
    - - TODO
      - 2017-11-06T23:32:56.190474317Z
  - header: Be smart about it
    state-history:
    - - TODO
      - 2017-11-06T23:33:05.478947389Z
```

This format is very easy to work with programmatically, and convenience
functions in the `smos-data` library are provided to operate on Smos data.

## Great for version control

Files are made to make git diffs easy and disjoint.
Derived data is never stored, but instead computed or rendered with `smos-report`.

## Installation

Clone the repository:

```
$ git clone https://github.com/NorfairKing/smos
```

Use [Stack](haskellstack.org) to install Smos with the default configuration:

```
$ stack install :smos
```

Should you wish to change the key controls, you can do this by making your own
little Haskell project (also with stack), define the configuration you want to
use and pass it to the [`smos`](https://github.com/NorfairKing/smos/blob/development/smos/src/Smos.hs#L29)
library function.

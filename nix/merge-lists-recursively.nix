{ lib, ... }:
attrList:
lib.foldr
  (
    x: y:
    lib.recursiveUpdate x y
  )
{ }
  attrList

{ lib, ... }:
attrList:
lib.fold
  (
    x: y:
    lib.recursiveUpdate x y
  )
{ }
  attrList

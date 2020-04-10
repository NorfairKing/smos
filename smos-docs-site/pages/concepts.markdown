---
title: SMOS Concepts
date: 2020-04-08
---

GTD
: GTD is a time management method by David Allen (see [Getting This
  Done](https://en.wikipedia.org/wiki/Getting_Things_Done) on
  Wikipedia).
  SMOS is supposed to be a tool to implement GTD.


<!-- the next concepts are related to SMOS configuration, but AFAIK
are not explained -->

Workflow
: What's the meaning in SMOS and why the need for a directory?


Projects
: Whats the meaning in SMOS?  Why the need for a directory in the
  configuration file?


Archive:
: When task and projects are completed, they are archived (i.e. moved
  in the archive directory).

State File
: ???

Entry
: In SMOS each entry can be in one of the following states: <!-- not true. an entry can have no state -->

	- WAITING
	- TODO
	- STARTED
	- READY
	- NEXT
	- FAILED
	- DONE
	- CANCELLED


Forest
: In SMOS each forest can be in one of the following states: <!-- not true, a forest can have no state  -->

	- WAITING
	- TODO
	- STARTED
	- READY
	- NEXT
	- FAILED
	- DONE
	- CANCELLED


Tag
: It is possible to attach a tag to each entry or forest.  Predefined
  tags are: `code`, `external`, `home`, `offline`, `online`, `power`,
  `toast` and `work`.  It is possible to define new tags simply with
  the [entrySelectTags](/configuration-reference.html#entrySelectTags)
  command


Property
: It is possible to attach a property with a value to entries or
  forests.  Predefined properties are: `brainpower`, `client`,
  `timewindow`.  It is not possible to define new properties. <!--
  Why? -->

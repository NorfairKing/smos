---
title: Smos Concepts
date: 2020-04-08
---


<!-- the next concepts are related to SMOS configuration, but AFAIK
are not explained -->

Workflow Directory
: SMOS files can be everywhere on your disk, however the `smos-query`
  command, used to generate reports and gather informations, will
  consult only the *workflow* directory.  The defalt value is
  `~/workflow` however the value can be changed to better suite your
  needs using the configuration file (see te [Configuration
  reference](/configuration-reference.html)).


Projects Directory
: The *project* directory is by used the `smos-query projects` command
  to retrieve information about your projects.  The default value is
  `~/workflow/projects` (see the [Configuration
  reference](/configuration-reference.html) for customization).


Archive Directory
: When task and projects are completed, they are archived, moved in
  the *archive* directory, so that they can be ignored by the
  `smos-query` command.  The default value is `~/workflow/archive`
  (see the [Configuration reference](/configuration-reference.html)
  for customization).

<!-- State File -->
<!-- : ??? -->

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

<!-- TODO: document the order of the states.  The first state should
be TODO or WAITING.  What the difference between READY and NEXT? -->

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

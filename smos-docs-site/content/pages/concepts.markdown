---
title: Smos Concepts
date: 2020-04-08
---


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

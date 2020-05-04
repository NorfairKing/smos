---
title: Directories in a workflow
date: 2020-05-04
---

The `smos` TUI and the `smos-query` executable use certain directories to function correctly and efficiently.
Each of these directories can be reconfigured.

```
workflow
├── ...
├── projects
│   └── ...
└── archive
    ├── ...
    └── projects
        └── ...
```

## The Workflow directory

SMOS files can be everywhere on your disk, however the `smos-query` executable which used to generate reports and gather information will
consult only the *workflow* directory.
The default value is `~/workflow`.

## The projects directory

Within the workflow directory, projects will be stored in seperate files within the projects directory.
The default value is `~/workflow/projects`.
If the workflow directory is reconfigured, the projects directory will still be within the workflow directory by default.

## The Archive directory

Within the workflow directory, `smos-archive` can archive files.
Archived files will be put in the archive directory within the workflow directory.
The default value is `~/workflow/archive`.
If the workflow directory is reconfigured, the archive directory will still be within the workflow directory by default.

## The Archived projects directory

The archived projects directory is only used for certain `smos-query` queries.
It is where archived projects should be found.
The default value is `~/workflow/archive/projects`.
If the archive directory is reconfigured, the archived projects directory will still be within the archive directory by default.

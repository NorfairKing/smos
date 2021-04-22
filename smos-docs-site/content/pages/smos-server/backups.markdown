---
title: Automatic backups of user' files
description: Documentation about automatic backups of users' files, for server administrators
---

By default, the smos server makes automatic backups of users' files.

Every hour, by default, the server checks whether any backups need to be made.
A backup is made when the latest backup was made more than a day ago.


Administrators can configure a maximum number of backups that any user can have.
When this number has been reached, the oldest backups will be deleted.
This deletion happens once every 24 hours by default.


See [the reference documentation for the `smos-server` for information on how to configure it](/smos-server).

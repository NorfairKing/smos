---
title: Email command
description: Documentation about the smos-jobhunt email command, for initiating an application project by sending an email
---

Run the `smos-jobhunt email` command to send an application email and keep track of the project.

Example invocation:

```
smos-jobhunt email syd@cs-syd.eu --name "Tom Sydney Kerckhove" --company "CS SYD" --url "https://cs-syd.eu"
```

In order for this to work, you need to configure a few things.

1. Configure your SMTP server so that `smos-jobhunt` can send emails.
2. Configure your email templates so that `smos-jobhunt` can compose an email for you.
    1. Subject template
    2. Plain text body template
    3. HTML body template

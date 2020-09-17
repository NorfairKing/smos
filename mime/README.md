# Getting xdg-open to work

Copy the mime type definition over:

```
$ cp mime/smos.mime-type ~/.local/share/mime/packages/smos.xml
```

Copy the `.desktop` file over:

```
$ cp mime/smos.desktop ~/.local/share/applications/smos.desktop
```

Update the mime database:

```
update-mime-database ~/.local/share/mime
```

Update the application database:

```
$ update-desktop-database ~/.local/share/applications
```

Try it out:

```
$ xdg-open milestones.smos
```

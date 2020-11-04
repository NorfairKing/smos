---
title: Database Migration
description: Documentation for what you need to do when the Smos Sync Client runs into a migration issue
---

When upgrading `smos-sync-client`, you may at times run into an error like this:

```
Database migration: manual intervention required.
The unsafe actions are prefixed by '***' below:

    CREATE TEMP TABLE "client_file_backup"("id" INTEGER PRIMARY KEY,"uuid" uuid NOT NULL,"path" VARCHAR NOT NULL,"sha256" BLOB NOT NULL,"time" INTEGER NOT NULL,CONSTRAINT "unique_path" UNIQUE ("path"),CONSTRAINT "unique_u_u_i_d" UNIQUE ("uuid"));
    INSERT INTO "client_file_backup"("id","uuid","path","sha256","time") SELECT "id","uuid","path","sha256","time" FROM "client_file";
*** DROP TABLE "client_file";
    CREATE TABLE "client_file"("id" INTEGER PRIMARY KEY,"uuid" uuid NOT NULL,"path" VARCHAR NOT NULL,"sha256" BLOB NOT NULL,"time" INTEGER NOT NULL,CONSTRAINT "unique_path" UNIQUE ("path"),CONSTRAINT "unique_u_u_i_d" UNIQUE ("uuid"));
    INSERT INTO "client_file" SELECT "id","uuid","path","sha256","time" FROM "client_file_backup";
    DROP TABLE "client_file_backup";

CallStack (from HasCallStack):
  error, called at ./Database/Persist/Sql/Migration.hs:101:14 in persistent-2.10.0-Jjk7XQn5Cwh2OYEvyhlR0s:Database.Persist.Sql.Migration
```

There are two ways to solve this problem,
but first you should backup your smos files well.

## Deleting the metadata database 

You'll need to remove both the metadata database **and** the server uuid file for this to work.
Otherwise all your files will be erased on the next sync.

```
rm ~/.smos/server-uuid.json 
rm ~/.smos/sync-metadata.sqlite3
```

Then sync again:

```
smos-sync-client sync
```

## Performing the database migration

Go into an `sqlite3` shell with the database:

```
sqlite3 ~/.smos/sync-metadata.sqlite3
```

Then run the commands listed in the error message one by one.
You'll need to remove the `***` in front of the dangerous commands to do so.


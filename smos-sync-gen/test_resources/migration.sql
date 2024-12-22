CREATE TABLE "client_file"("id" INTEGER PRIMARY KEY,"path" VARCHAR NOT NULL,"sha256" BLOB NOT NULL,"time" INTEGER NOT NULL,CONSTRAINT "unique_path" UNIQUE ("path"));

-- ATTENTION CODE REVIEWER
-- If this file has been updated, please make sure to check
-- whether this test failed before that happened:
-- "Smos.Sync.Client.DBSpec.Can automatically migrate from the previous database schema"
-- If this test failed beforehand, but this golden test has
-- been updated anyway, that means the current migration is
-- dangerous with respect to the current database.

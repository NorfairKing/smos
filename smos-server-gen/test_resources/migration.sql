CREATE TABLE "user"("id" INTEGER PRIMARY KEY,"name" VARCHAR NOT NULL,"hashed_password" VARCHAR NOT NULL,"created" TIMESTAMP NOT NULL,"last_login" TIMESTAMP NULL DEFAULT NULL,"last_use" TIMESTAMP NULL DEFAULT NULL,CONSTRAINT "unique_username" UNIQUE ("name"));
CREATE TABLE "stripe_customer"("id" INTEGER PRIMARY KEY,"user" INTEGER NOT NULL REFERENCES "user" ON DELETE RESTRICT ON UPDATE RESTRICT,"customer" VARCHAR NOT NULL,CONSTRAINT "unique_stripe_customer" UNIQUE ("user","customer"));
CREATE TABLE "subscription"("id" INTEGER PRIMARY KEY,"user" INTEGER NOT NULL REFERENCES "user" ON DELETE RESTRICT ON UPDATE RESTRICT,"end" TIMESTAMP NOT NULL,CONSTRAINT "unique_subscription_user" UNIQUE ("user"));
CREATE TABLE "server_file"("id" INTEGER PRIMARY KEY,"user" INTEGER NOT NULL REFERENCES "user" ON DELETE RESTRICT ON UPDATE RESTRICT,"path" VARCHAR NOT NULL,"contents" BLOB NOT NULL,"time" INTEGER NOT NULL,CONSTRAINT "unique_server_file_path" UNIQUE ("user","path"));
CREATE TABLE "backup"("id" INTEGER PRIMARY KEY,"user" INTEGER NOT NULL REFERENCES "user" ON DELETE RESTRICT ON UPDATE RESTRICT,"uuid" BLOB NOT NULL,"time" TIMESTAMP NOT NULL,"size" INTEGER NOT NULL,CONSTRAINT "unique_backup_u_u_i_d" UNIQUE ("user","uuid"));
CREATE TABLE "backup_file"("id" INTEGER PRIMARY KEY,"backup" INTEGER NOT NULL REFERENCES "backup" ON DELETE RESTRICT ON UPDATE RESTRICT,"path" VARCHAR NOT NULL,"contents" BLOB NOT NULL);

-- ATTENTION CODE REVIEWER
-- If this file has been updated, please make sure to check
-- whether this test failed before that happened:
-- "Smos.Server.DBSpec.Can automatically migrate from the previous database schema"
-- If this test failed beforehand, but this golden test has
-- been updated anyway, that means the current migration is
-- dangerous with respect to the current database.

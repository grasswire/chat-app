# Building the project

You will need some native C libraries to build project dependencies.

On Mac OS, you can run:

`brew install icu4c`

`brew link icu4c --force`

Build the project using `stack`:

`stack build`

Run the project using `stack`:

`stack exec --chat-app`

If you need to compile JS/CSS assets, you can run the `start` script:

`./script`

which will compile all assets into the `/static` directory, build the project and start the server.

# Preparing the application database 

Using psql, do the following

```
levinotik=# create user taplike with password 'taplike';
CREATE ROLE
levinotik=# create database taplike;
CREATE DATABASE
levinotik=# create database taplike_test;
CREATE DATABASE
levinotik=# grant all privileges on database taplike to taplike;
GRANT
levinotik=# grant all privileges on database taplike_test to taplike;
GRANT
```

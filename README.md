You will need some native C libraries to build project dependencies.

On Mac OS, you can run:

`brew link icu4c --force`


using psql, do the following

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

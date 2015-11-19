# Dependencies

## Mac

```
brew install postgresql
```

## Ubuntu

```
sudo apt-get install libpq-dev
```

# Building

```
stack build
```

# Prepping the database

Using psql, do the following

```
$ psql -d template1
you=# create user taplike with password 'taplike';
CREATE ROLE
you=# create database taplike;
CREATE DATABASE
you=# create database taplike_test;
CREATE DATABASE
you=# create database chat-app_test;
CREATE DATABASE
you=# grant all privileges on database taplike to taplike;
GRANT
you=# grant all privileges on database taplike_test to taplike;
GRANT
```

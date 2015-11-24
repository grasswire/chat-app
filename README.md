# Dependencies

## Node.js

https://nodejs.org/en/blog/release/v4.2.0/

You'll need node, npm, and gulp in your path in order for the `./start` init script for frontend assets to work.

## Mac

```
brew install postgresql
```

## Ubuntu

```
sudo apt-get install postgresql libpq-dev
```

# Building

```
stack build
```

# Prepping the database

Using psql, do the following

```
$ sudo su - postgres
$ psql -d template1
you=# create user taplike with password 'taplike';
CREATE ROLE
you=# create database taplike;
CREATE DATABASE
you=# create database taplike_test;
CREATE DATABASE
you=# create database "chat-app_test";
CREATE DATABASE
you=# grant all privileges on database taplike to taplike;
GRANT
you=# grant all privileges on database taplike_test to taplike;
GRANT
```

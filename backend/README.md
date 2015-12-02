# Building and Running the Project

## stack

`Stack` "is a cross-platform program for developing Haskell projects". You will use it to build the
project. You can find instructions for installing `stack` on your OS [here](http://docs.haskellstack.org/en/stable/README.html)

## Setup

### Stack setup

In the project directory, run `stack setup` which will download the compiler if necessary. You only need to run this command the first time you build the project.

### Preparing the application database

Using `psql`, do the following:

```
me=# create user taplike with password 'taplike';
CREATE ROLE
me=# create database taplike;
CREATE DATABASE
me=# create database taplike_test;
CREATE DATABASE
me=# grant all privileges on database taplike to taplike;
GRANT
me=# grant all privileges on database taplike_test to taplike;
GRANT
```

### Installing and running Redis

Installation:

**Mac OS**: `brew install redis`
**Ubuntu**: `sudo apt-get install redis-server`

Running Redis server: `redis-server`


## Build

Before you can build the project, you will need some native libraries available on your system if they aren't already installed.

On **Mac** OS, you can run:

`brew install icu4c`
`brew link icu4c --force`

*Note: this may cause an error when you go to build, see the Notes section below if you can't get stack to build.*

On **Ubuntu**:

`sudo apt-get install libicu-dev libpq-dev libtinfo-dev -y`

There are two ways to build and run the project.

### Method 1: build and run with start script

Make sure the start script is executable. `chmod +x start` then run it:

`./start`

This script installs all required `node` packages, uses `gulp` to compile JS and CSS assets and starts the server. If you edit any source files and save your changes, the project will recompile and start the server again.

### Method 2: build and run with separate commands

`gulp compile`

`stack build`

`stack exec --chat-app`

### Notes

If GHC complains that it still cannot find icuuc, you might see somthing like this in the error: `Library not loaded: /usr/local/opt/icu4c/lib/libicuuc.55.dylib`

Try the following to fix it:

```
$ brew unlink icu4c
$ brew uninstall icu4c
$ brew install https://raw.githubusercontent.com/Homebrew/homebrew/e52ee0523e9f9bf94b8bb50bbcb426e120d20069/Library/Formula/icu4c.rb
$ brew link icu4c --force
```

All we've done here is removed the latest version of icu4c and installed the version we know is working (55.1) and re-linked.  After you've done this you should be able to run the project with no issue.

If the above doesn't work you can explicitly let `stack build` know where it is. For example, on Mac:

```stack build --extra-include-dirs=/usr/local/opt/icu4c/include --extra-lib-dirs=/usr/local/opt/icu4c/lib
```


# Getting started

## Stack version

You want to be at least on Stack 0.1.7.0, check with `stack --version`. If you need to upgrade:

```
stack upgrade
```

## Getting ghcjs installed

### NodeJS

#### Ubuntu

You need 0.10.28 or newer.

```
sudo add-apt-repository ppa:chris-lea/node.js
sudo apt-get update
sudo apt-get install nodejs
```
#### Mac OS X

```
brew install node
```

### Stack

This will take awhile due to the ghcjs-boot process.

```
stack setup
```

Outside of the project dir, you want to build it with GHC.

```
stack install happy
```

Back inside `frontend/`.

```
stack build
```

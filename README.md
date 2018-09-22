# dotfiles

## What is this?

Herein lies configuration files and scripts for installing them onto
OSX (Apple Mac operating system). 


## Quick Start

### On a fresh machine

```bash
sh install.sh
```

### Just configuration

```bash
sh setup.sh
```

NOTE: `setup.sh` assumes that `install.sh` has run and completed
successfully.


## Motivation

What started as a collection of application configuration files has
morphed into a set of scripts for turning a clean machine into a
customized software development environment. It's customized for my
work, but could be adapted for you.


## Concepts

These dotfiles have 2 phases:

+ install phase
+ setup phase

### Install Phase

As you would expect, the _install phase_ moves files around. It
installs then runs _Homebrew_ (many times!), it downloads applications
and installs all the applications. The details of which applications
are installed are in the `install/` directory. The subdirectories of
`install/` indicate explicit order thus handling dependencies.


### Setup Phase

The _setup phase_ deals with configuration. It generates and sets up
symlinks in the host filesystem for applications to access the
configuration.

The configuration files (or the scripts that generate them) all exist
within this git repo. Nothing new is downloaded or installed in this
phase.

The _setup phase_ scripts can be found in `setup/`. Unlike the
_install phase_, this phase has no concept of order or dependencies.
It assumes that symlink'ing configuration files shouldn't depend on
any other.

#### Caveat

As you've probably noticed, not all the configuration files are in the
`setup/` directory. I'd like to get them there, but they're not there
yet.

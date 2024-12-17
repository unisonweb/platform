The Unison language
===================

[![CI Status](https://github.com/unisonweb/unison/actions/workflows/ci.yaml/badge.svg)](https://github.com/unisonweb/unison/actions/workflows/ci.yaml?query=branch%3Atrunk)
[![Pre-Release Status](https://github.com/unisonweb/unison/actions/workflows/pre-release.yaml/badge.svg)](https://github.com/unisonweb/unison/actions/workflows/pre-release.yaml)

* [Overview](#overview)
* [Building using Stack](#building-using-stack)
* [Language Server Protocol (LSP)](docs/language-server.markdown)
* [Codebase Server](#codebase-server)
* [Configuration](./docs/configuration.md)

![Alt](https://repobeats.axiom.co/api/embed/92b662a65fd842d49cb8d7d813043f5f5b4b550d.svg "Repobeats analytics image")

Overview
--------

[Unison](https://unisonweb.org) is a modern, statically-typed purely functional language with the ability to describe entire distributed systems using a single program. Here's an example of a distributed map-reduce implementation:

```Haskell
-- comments start with `--`
mapReduce loc fn ifEmpty reduce data = match split data with
  Empty          -> ifEmpty
  One a          -> fn a
  Two left right ->
    fl = forkAt loc '(mapReduce loc fn ifEmpty reduce !left)
    fr = forkAt loc '(mapReduce loc fn ifEmpty reduce !right)
    reduce (await fl) (await fr)
```

This function can be either simulated locally (possibly with faults injected for testing purposes), or run atop a distributed pool of compute. See [this article](https://www.unison-lang.org/articles/distributed-datasets/) for more in-depth coverage of how to build distributed computing libraries like this.

Other resources:

* [Learn about the big idea behind Unison](https://www.unison-lang.org/learn/the-big-idea/)
* Check out [the project website](https://unison-lang.org)
* Say hello or lurk [in the Discord chat](https://unison-lang.org/discord)
* Explore [the Unison ecosystem](https://share.unison-lang.org/)
* [Learn Unison](https://www.unison-lang.org/learn/)

Building using Stack
--------------------

If these instructions don't work for you or are incomplete, please file an issue.

The build uses [Stack](http://docs.haskellstack.org/). If you don't already have it installed, [follow the install instructions](http://docs.haskellstack.org/en/stable/README.html#how-to-install) for your platform.  (Hint: `brew update && brew install stack`)

```sh
$ git clone https://github.com/unisonweb/unison.git
$ cd unison
$ stack --version # we'll want to know this version if you run into trouble
$ stack build --fast --test && stack exec unison
```

To run the Unison Local UI while building from source, you can use the `/dev-ui-install.sh` script. It will download the latest release of [unison-local-ui](https://github.com/unisonweb/unison-local-ui) and put it in the expected location for the unison executable created by `stack build`. When you start unison, you'll see a url where Unison Local UI is running.

See [`development.markdown`](development.markdown) for a list of build commands you'll likely use during development.

Language Server Protocol (LSP)
------------------------------

View Language Server setup instructions [here](docs/language-server.markdown).

Codebase Server
---------------

When `ucm` starts it starts a Codebase web server that is used by the
[Unison Local UI](https://github.com/unisonweb/unison-local-ui). It selects a random
port and a unique token that must be used when starting the UI to correctly
connect to the server.

The port, host and token can all be configured by providing environment
variables when starting `ucm`: `UCM_PORT`, `UCM_HOST`, and `UCM_TOKEN`.

Configuration
-------------

See the documentation for configuration [here](docs/configuration.md)

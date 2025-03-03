memory
======

[![Build Status](https://travis-ci.org/vincenthz/hs-memory.png?branch=master)](https://travis-ci.org/vincenthz/hs-memory)
[![BSD](http://b.repl.ca/v1/license-BSD-blue.png)](http://en.wikipedia.org/wiki/BSD_licenses)
[![Haskell](http://b.repl.ca/v1/language-haskell-lightgrey.png)](http://haskell.org)

Documentation: [memory on hackage](http://hackage.haskell.org/package/memory)

A generic memory and related abstraction for haskell:

* A polymorphic byte array abstraction and function similar to strict ByteString.
* Different type of byte array abstraction.
* Raw memory IO operations (memory set, memory copy, ..)
* Aliasing with endianness support.

Also provides some useful helpers:

* Fast Hashing : [SipHash](https://131002.net/siphash/), [FNV1](http://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function).
* Built-in base encoding : Base16, Base32, [Base64](http://en.wikipedia.org/wiki/Base64).

Versioning
----------

Development versions are an incremental number prefixed by 0.
No specific meaning is associated with the versions, specially
no API stability.

Production versions : TBD

Coding Style
------------

The coding style of this project mostly follows:
[haskell-style](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md)

Support
-------

See [Haskell packages guidelines](https://github.com/vincenthz/haskell-pkg-guidelines/blob/master/README.md#support)

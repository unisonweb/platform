# uri-encode
[![Build Status](https://travis-ci.org/silkapp/uri-encode.svg?branch=master)](https://travis-ci.org/silkapp/uri-encode)

This package allows you to uri encode and uri decode `String`s,
`Text`s and `ByteString`s.

The default is to encode everything but ASCII alphabetic characters,
decimal digits, and `- _ . ~`, according to RFC 3986.

It has support for all of unicode, by first encoding strings to UTF8,
and then encoding the individual bytes. This works both for `network`
\> 2.4 (which also does this) and for older version.

Additionally, two command line utilities are provided if the package
is built with the `tools` flag: `uri-encode` and `uri-decode`.

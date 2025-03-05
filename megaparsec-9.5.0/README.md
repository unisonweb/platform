# Megaparsec

[![License FreeBSD](https://img.shields.io/badge/license-FreeBSD-brightgreen.svg)](http://opensource.org/licenses/BSD-2-Clause)
[![Hackage](https://img.shields.io/hackage/v/megaparsec.svg?style=flat)](https://hackage.haskell.org/package/megaparsec)
[![Stackage Nightly](http://stackage.org/package/megaparsec/badge/nightly)](http://stackage.org/nightly/package/megaparsec)
[![Stackage LTS](http://stackage.org/package/megaparsec/badge/lts)](http://stackage.org/lts/package/megaparsec)
![CI](https://github.com/mrkkrp/megaparsec/workflows/CI/badge.svg?branch=master)

* [Features](#features)
    * [Core features](#core-features)
    * [Error messages](#error-messages)
    * [External lexers](#external-lexers)
    * [Character and binary parsing](#character-and-binary-parsing)
    * [Lexer](#lexer)
* [Documentation](#documentation)
* [Tutorials](#tutorials)
* [Performance](#performance)
* [Comparison with other solutions](#comparison-with-other-solutions)
    * [Megaparsec vs Attoparsec](#megaparsec-vs-attoparsec)
    * [Megaparsec vs Parsec](#megaparsec-vs-parsec)
    * [Megaparsec vs Trifecta](#megaparsec-vs-trifecta)
    * [Megaparsec vs Earley](#megaparsec-vs-earley)
* [Related packages](#related-packages)
* [Prominent projects that use Megaparsec](#prominent-projects-that-use-megaparsec)
* [Links to announcements and blog posts](#links-to-announcements-and-blog-posts)
* [Contribution](#contribution)
* [License](#license)

This is an industrial-strength monadic parser combinator library. Megaparsec
is a feature-rich package that tries to find a nice balance between speed,
flexibility, and quality of parse errors.

## Features

The project provides flexible solutions to satisfy common parsing needs. The
section describes them shortly. If you're looking for comprehensive
documentation, see the [section about documentation](#documentation).

### Core features

The package is built around `MonadParsec`, an MTL-style monad transformer.
Most features work with all instances of `MonadParsec`. One can achieve
various effects combining monad transformers, i.e. building a monadic stack.
Since the common monad transformers like `WriterT`, `StateT`, `ReaderT` and
others are instances of the `MonadParsec` type class, one can also wrap
`ParsecT` *in* these monads, achieving, for example, backtracking state.

On the other hand `ParsecT` is an instance of many type classes as well. The
most useful ones are `Monad`, `Applicative`, `Alternative`, and
`MonadParsec`.

Megaparsec includes all functionality that is typically available in
Parsec-like libraries and also features some special combinators:

* `parseError` allows us to end parsing and report an arbitrary parse error.
* `withRecovery` can be used to recover from parse errors “on-the-fly” and
  continue parsing. Once parsing is finished, several parse errors may be
  reported or ignored altogether.
* `observing` makes it possible to “observe” parse errors without ending
  parsing.

In addition to that, Megaparsec features high-performance combinators
similar to those found in [Attoparsec][attoparsec]:

* `tokens` makes it easy to parse several tokens in a row (`string` and
  `string'` are built on top of this primitive). This is about 100 times
  faster than matching a string token by token. `tokens` returns “chunk” of
  original input, meaning that if you parse `Text`, it'll return `Text`
  without repacking.
* `takeWhile` and `takeWhile1` are about 150 times faster than approaches
  involving `many`, `manyTill` and other similar combinators.
* `takeP` allows us to grab n tokens from the stream and returns them as a
  “chunk” of the stream.

Megaparsec is about as fast as Attoparsec if you write your parser carefully
(see also [the section about performance](#performance)).

The library can currently work with the following types of input stream
out-of-the-box:

* `String = [Char]`
* `ByteString` (strict and lazy)
* `Text` (strict and lazy)

It's also possible to make it work with custom token streams by making them
an instance of the `Stream` type class.

### Error messages

* Megaparsec has typed error messages and the ability to signal custom parse
  errors that better suit the user's domain of interest.

* Since version 8, the location of parse errors can independent of current
  offset in the input stream. It is useful when you want a parse error to
  point to a particular position after performing some checks.

* Instead of a single parse error Megaparsec produces so-called
  `ParseErrorBundle` data type that helps to manage multi-error messages and
  pretty-print them. Since version 8, reporting multiple parse errors at
  once has become easier.

### External lexers

Megaparsec works well with streams of tokens produced by tools like Alex.
The design of the `Stream` type class has been changed significantly in the
recent versions, but user can still work with custom streams of tokens.

### Character and binary parsing

Megaparsec has decent support for Unicode-aware character parsing. Functions
for character parsing live in the [`Text.Megaparsec.Char`][tm-char] module.
Similarly, there is [`Text.Megaparsec.Byte`][tm-byte] module for parsing
streams of bytes.

### Lexer

[`Text.Megaparsec.Char.Lexer`][tm-char-lexer] is a module that should help
you write your lexer. If you have used `Parsec` in the past, this module
“fixes” its particularly inflexible `Text.Parsec.Token`.

[`Text.Megaparsec.Char.Lexer`][tm-char-lexer] is intended to be imported
using a qualified import, it's not included in [`Text.Megaparsec`][tm]. The
module doesn't impose how you should write your parser, but certain
approaches may be more elegant than others. An especially important theme is
parsing of white space, comments, and indentation.

The design of the module allows one quickly solve simple tasks and doesn't
get in the way when the need to implement something less standard arises.

[`Text.Megaparsec.Byte.Lexer`][tm-byte-lexer] is also available for users
who wish to parse binary data.

## Documentation

Megaparsec is well-documented. See the [current version of Megaparsec
documentation on Hackage][hackage].

## Tutorials

You can find the most complete Megaparsec tutorial [here][the-tutorial]. It
should provide sufficient guidance to help you start with your parsing
tasks.

## Performance

Despite being flexible, Megaparsec is also fast. Here is how Megaparsec
compares to [Attoparsec][attoparsec] (the fastest widely used parsing
library in the Haskell ecosystem):

Test case         | Execution time | Allocated | Max residency
------------------|---------------:|----------:|-------------:
CSV (Attoparsec)  |       76.50 μs |   397,784 |        10,544
CSV (Megaparsec)  |       64.69 μs |   352,408 |         9,104
Log (Attoparsec)  |       302.8 μs | 1,150,032 |        10,912
Log (Megaparsec)  |       337.8 μs | 1,246,496 |        10,912
JSON (Attoparsec) |       18.20 μs |   128,368 |         9,032
JSON (Megaparsec) |       25.45 μs |   203,824 |         9,176

You can run the benchmarks yourself by executing:

```
$ nix-build -A benches.parsers-bench
$ cd result/bench
$ ./bench-memory
$ ./bench-speed
```

More information about benchmarking and development can be found
[here][hacking].

## Comparison with other solutions

There are quite a few libraries that can be used for parsing in Haskell,
let's compare Megaparsec with some of them.

### Megaparsec vs Attoparsec

[Attoparsec][attoparsec] is another prominent Haskell library for parsing.
Although both libraries deal with parsing, it's usually easy to decide which
you will need in particular project:

* *Attoparsec* is sometimes faster but not that feature-rich. It should be
  used when you want to process large amounts of data where performance
  matters more than quality of error messages.

* *Megaparsec* is good for parsing of source code or other human-readable
  texts. It has better error messages and it's implemented as a monad
  transformer.

So, if you work with something human-readable where the size of input data
is moderate, it makes sense to go with Megaparsec, otherwise Attoparsec may
be a better choice.

### Megaparsec vs Parsec

Since Megaparsec is a fork of [Parsec][parsec], we are bound to list the
main differences between the two libraries:

* Better error messages. Megaparsec has typed error messages and custom
  error messages, it can also report multiple parse errors at once.

* Megaparsec can show the line on which parse error happened as part of
  parse error. This makes it a lot easier to figure out where the error
  happened.

* Some quirks and bugs of Parsec are fixed.

* Better support for Unicode parsing in [`Text.Megaparsec.Char`][tm-char].

* Megaparsec has more powerful combinators and can parse languages where
  indentation matters.

* Better documentation.

* Megaparsec can recover from parse errors “on the fly” and continue
  parsing.

* Megaparsec allows us to conditionally process parse errors inside a
  running parser. In particular, it's possible to define regions in which
  parse errors, should they happen, will get a “context tag”, e.g. we could
  build a context stack like “in function definition foo”, “in expression
  x”, etc.

* Megaparsec is faster and supports efficient operations `tokens`,
  `takeWhileP`, `takeWhile1P`, `takeP`, like Attoparsec.

If you want to see a detailed change log, `CHANGELOG.md` may be helpful.
Also see [this original announcement][original-announcement] for another
comparison.

### Megaparsec vs Trifecta

[Trifecta][trifecta] is another Haskell library featuring good error
messages. These are the common reasons why Trifecta may be problematic to
use:

* Complicated, doesn't have any tutorials available, and documentation
  doesn't help much.

* Trifecta can parse `String` and `ByteString` natively, but not `Text`.

* Depends on `lens`, which is a very heavy dependency. If you're not into
  `lens`, you may not like the API.

[Idris][idris] has switched from Trifecta to Megaparsec which allowed it to
[have better error messages and fewer dependencies][idris-testimony].

### Megaparsec vs Earley

[Earley][earley] is a newer library that allows us to safely parse
context-free grammars (CFG). Megaparsec is a lower-level library compared to
Earley, but there are still enough reasons to choose it:

* Megaparsec is faster.

* Your grammar may be not context-free or you may want introduce some sort
  of state to the parsing process. Almost all non-trivial parsers require
  state. Even if your grammar is context-free, state may allow for
  additional niceties. Earley does not support that.

* Megaparsec's error messages are more flexible allowing to include
  arbitrary data in them, return multiple error messages, mark regions that
  affect any error that happens in those regions, etc.

In other words, Megaparsec is less safe but also more powerful.

## Related packages

The following packages are designed to be used with Megaparsec (open a PR if
you want to add something to the list):

* [`hspec-megaparsec`](https://hackage.haskell.org/package/hspec-megaparsec)—utilities
  for testing Megaparsec parsers with with
  [Hspec](https://hackage.haskell.org/package/hspec).
* [`replace-megaparsec`](https://hackage.haskell.org/package/replace-megaparsec)—Stream
  editing and find-and-replace with Megaparsec.
* [`cassava-megaparsec`](https://hackage.haskell.org/package/cassava-megaparsec)—Megaparsec
  parser of CSV files that plays nicely with
  [Cassava](https://hackage.haskell.org/package/cassava).
* [`tagsoup-megaparsec`](https://hackage.haskell.org/package/tagsoup-megaparsec)—a
  library for easily using
  [TagSoup](https://hackage.haskell.org/package/tagsoup) as a token type in
  Megaparsec.
* [`parser-combinators`](https://hackage.haskell.org/package/parser-combinators)—provides permutation and expression parsers [previously bundled with Megaparsec](https://markkarpov.com/post/megaparsec-7.html#parsercombinators-grows-megaparsec-shrinks).
* [`faster-megaparsec`](https://hackage.haskell.org/package/faster-megaparsec)—speeds up parsing
  by trying a simple `MonadParsec` instance and falls back to `ParsecT` to report errors.

## Prominent projects that use Megaparsec

Some prominent projects that use Megaparsec:

* [Idris](https://github.com/idris-lang/Idris-dev)—a general-purpose
  functional programming language with dependent types
* [Dhall](https://github.com/dhall-lang/dhall-haskell)—an advanced
  configuration language
* [hnix](https://github.com/haskell-nix/hnix)—re-implementation of the Nix
  language in Haskell
* [Hledger](https://github.com/simonmichael/hledger)—an accounting tool
* [MMark](https://github.com/mmark-md/mmark)—strict markdown processor for
  writers

## Links to announcements and blog posts

Here are some blog posts mainly announcing new features of the project and
describing what sort of things are now possible:

* [Megaparsec 8](https://markkarpov.com/post/megaparsec-8.html)
* [Megaparsec 7](https://markkarpov.com/post/megaparsec-7.html)
* [Evolution of error messages](https://markkarpov.com/post/evolution-of-error-messages.html)
* [A major upgrade to Megaparsec: more speed, more power](https://markkarpov.com/post/megaparsec-more-speed-more-power.html)
* [Latest additions to Megaparsec](https://markkarpov.com/post/latest-additions-to-megaparsec.html)
* [Announcing Megaparsec 5](https://markkarpov.com/post/announcing-megaparsec-5.html)
* [Megaparsec 4 and 5](https://markkarpov.com/post/megaparsec-4-and-5.html)
* [The original Megaparsec 4.0.0 announcement][original-announcement]

## Contribution

Issues (bugs, feature requests or otherwise feedback) may be reported in
[the GitHub issue tracker for this
project](https://github.com/mrkkrp/megaparsec/issues).

Pull requests are also welcome. If you would like to contribute to the
project, you may find [this document][hacking] helpful.

## License

Copyright © 2015–present Megaparsec contributors\
Copyright © 2007 Paolo Martini\
Copyright © 1999–2000 Daan Leijen

Distributed under FreeBSD license.

[hackage]: https://hackage.haskell.org/package/megaparsec
[the-tutorial]: https://markkarpov.com/tutorial/megaparsec.html
[hacking]: ./HACKING.md

[tm]: https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec.html
[tm-char]: https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Char.html
[tm-byte]: https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Byte.html
[tm-char-lexer]: https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Char-Lexer.html
[tm-byte-lexer]: https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Byte-Lexer.html

[attoparsec]: https://hackage.haskell.org/package/attoparsec
[parsec]: https://hackage.haskell.org/package/parsec
[trifecta]: https://hackage.haskell.org/package/trifecta
[earley]: https://hackage.haskell.org/package/Earley
[idris]: https://www.idris-lang.org/
[idris-testimony]: https://twitter.com/edwinbrady/status/950084043282010117?s=09

[parsers-bench]: https://github.com/mrkkrp/parsers-bench
[fast-parser]: https://markkarpov.com/megaparsec/writing-a-fast-parser.html
[original-announcement]: https://mail.haskell.org/pipermail/haskell-cafe/2015-September/121530.html

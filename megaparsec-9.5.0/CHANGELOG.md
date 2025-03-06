*Megaparsec follows [SemVer](https://semver.org/).*

## Megaparsec 9.5.0

* Dropped a number of redundant constraints here and there. [PR
  523](https://github.com/mrkkrp/megaparsec/pull/523).

* Added a `MonadWriter` instance for `ParsecT`. [PR
  534](https://github.com/mrkkrp/megaparsec/pull/534).

## Megaparsec 9.4.1

* Removed `Monad m` constraints in several places where they were introduced
  in 9.4.0. [Issue 532](https://github.com/mrkkrp/megaparsec/issues/532).

## Megaparsec 9.4.0

* `dbg` now prints hints among other debug information. [PR
  530](https://github.com/mrkkrp/megaparsec/pull/530).

* Hints are no longer lost in certain methods of MTL instances for
  `ParsecT`. [Issue 528](https://github.com/mrkkrp/megaparsec/issues/528).

* Added a new method to the `MonadParsec` type class—`mkParsec`. This can be
  used to construct “new primitives” with arbitrary behavior at the expense
  of having to dive into Megaparsec's internals. [PR
  514](https://github.com/mrkkrp/megaparsec/pull/514).

## Megaparsec 9.3.1

* Fixed a bug related to processing of tabs when error messages are
  rendered. [Issue 524](https://github.com/mrkkrp/megaparsec/issues/524).

## Megaparsec 9.3.0

* Now `label` can override more than one group of hints in the parser it
  wraps. [Issue 482](https://github.com/mrkkrp/megaparsec/issues/482).

* `takeP n` now returns the empty chunk of the input stream when `n` is
  negative, similar to when `n == 0`. [Issue
  497](https://github.com/mrkkrp/megaparsec/issues/497).

* Added the `MonadParsecDbg` type class in `Text.Megaparsec.Debug`. The type
  class allows us to use `dbg` in MTL monad transformers. [Issue
  488](https://github.com/mrkkrp/megaparsec/issues/488).

* Introduced the `ShareInput` and `NoShareInput` newtype wrappers in
  `Text.Megaparsec.Stream` in order to allow the user to choose how the
  input should be sliced and shared during the parsing. [Issue
  492](https://github.com/mrkkrp/megaparsec/issues/492).

## Megaparsec 9.2.2

* Fixed a space leak in the implementations of the `reachOffset` and
  `reachOffsetNoLine` methods of `TraversableStream`. [Issue
  486](https://github.com/mrkkrp/megaparsec/issues/486).

## Megaparsec 9.2.1

* Builds with `mtl-2.3` and `transformers-0.6`.

## Megaparsec 9.2.0

* Added parsers for binary representations (little/big endian) of numbers in
  `Text.Megaparsec.Byte.Binary`.

## Megaparsec 9.1.0

* Added `dbg'` in `Text.Megaparsec.Debug` for debugging parsers that have
  unshowable return values.

* Documentation improvements.

## Megaparsec 9.0.1

* Added [Safe
  Haskell](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/safe_haskell.html)
  support.

## Megaparsec 9.0.0

* Split the `Stream` type class. The methods `showTokens` and `tokensLength`
  have been put into a separate type class `VisualStream`, while
  `reachOffset` and `reachOffsetNoLine` are now in `TraversableStream`. This
  should make defining `Stream` instances for custom streams easier.

* Defined `Stream` instances for lists and `Seq`s.

* Added the functions `hspace` and `hspace1` to the `Text.Megaparsec.Char`
  and `Text.Megaparsec.Byte` modules.

## Megaparsec 8.0.0

* The methods `failure` and `fancyFailure` of `MonadParsec` are now ordinary
  functions and live in `Text.Megaparsec`. They are defined in terms of the
  new `parseError` method of `MonadParsec`. This method allows us to signal
  parse errors at a given offset without manipulating parser state manually.

* Megaparsec now supports registration of “delayed” parse errors. On lower
  level we added a new field called `stateParseErrors` to the `State`
  record. The type also had to change from `State s` to `State s e`. This
  field contains the list of registered `ParseErrors` that do not end
  parsing immediately but still will cause failure in the end if the list is
  not empty. Users are expected to register parse errors using the three
  functions: `registerParseError`, `registerFailure`, and
  `registerFancyFailure`. These functions are analogous to those without the
  `register` prefix, except that they have “delayed” effect.

* Added the `tokensLength` method to the `Stream` type class to improve
  support for custom input streams.

* Added the `setErrorOffset` function to set offset of `ParseError`s.

* Changed type signatures of `reachOffset` and `reachOffsetNoLine` methods
  of the `Stream` type class. Instead of three-tuple `reachOffset` now
  returns two-tuple because `SourcePos` is already contained in the returned
  `PosState` record.

* Generalized `decimal`, `binary`, `octal`, and `hexadecimal` parsers in
  lexer modules so that they `Num` instead of just `Integral`.

* Dropped support for GHC 8.2.x and older.

## Megaparsec 7.0.5

* Dropped support for GHC 7.10.

* Adapted the code to `MonadFail` changes in `base-4.13`.

* Separated the test suite into its own package. The reason is that we can
  avoid circular dependency on `hspec-megaparsec` and thus avoid keeping
  copies of its source files in our test suite, as we had to do before.
  Another benefit is that we can export some auxiliary functions in
  `megaparsec-tests` which can be used by other test suites, for example in
  the `parser-combinators-tests` package.

  Version of `megaparsec-tests` will be kept in sync with versions of
  `megaparsec` from now on.

## Megaparsec 7.0.4

* Numerous documentation corrections.

## Megaparsec 7.0.3

* Fixed the build with `mtl` older than `2.2.2`.

## Megaparsec 7.0.2

* Fixed the property test for `char'` which was failing in the case when
  there is a character with different upper and title cases.

* More descriptive error messages when `elabel` or `ulabel` from
  `Text.Megaparsec.Error.Builder` are used with empty strings.

* Typo fixes in the docs.

## Megaparsec 7.0.1

* Fixed a bug in `errorBundlePretty`. Previously the question sign `?` was
  erroneously inserted before offending line in 2nd and later parse errors.

## Megaparsec 7.0.0

### General

* Dropped the `Text.Megaparsec.Perm` module. Use
  `Control.Applicative.Permutations` from `parser-combinators` instead.

* Dropped the `Text.Megaparsec.Expr` module. Use
  `Control.Monad.Combinators.Expr` from `parser-combinators` instead.

* The debugging function `dbg` has been moved from `Text.Megaparsec` to its
  own module `Text.Megaparsec.Debug`.

* Dropped support for GHC 7.8.

### Combinators

* Moved some general combinators from `Text.Megaparsec.Char` and
  `Text.Megaparsec.Byte` to `Text.Megaparsec`, renaming some of them for
  clarity.

  Practical consequences:

  * Now there is the `single` combinator that is a generalization of `char`
    for arbitrary streams. `Text.Megaparsec.Char` and `Text.Megaparsec.Byte`
    still contain `char` as type-constrained versions of `single`.

  * Similarly, now there is the `chunk` combinator that is a generalization
    of `string` for arbitrary streams. The `string` combinator is still
    re-exported from `Text.Megaparsec.Char` and `Text.Megaparsec.Byte` for
    compatibility.

  * `satisfy` does not depend on type of token, and so it now lives in
    `Text.Megaparsec`.

  * `anyChar` was renamed to `anySingle` and moved to `Text.Megaparsec`.

  * `notChar` was renamed to `anySingleBut` and moved to `Text.Megaparsec`.

  * `oneOf` and `noneOf` were moved to `Text.Megaparsec`.

* Simplified the type of the `token` primitive. It now takes just a matching
  function `Token s -> Maybe a` as the first argument and the collection of
  expected items `Set (ErrorItem (Token s))` as the second argument. This
  makes sense because the collection of expected items cannot depend on what
  we see in the input stream.

* The `label` primitive now doesn't prepend the phrase “the rest of” to the
  label when its inner parser produces hints after consuming input. In that
  case `label` has no effect.

* Fixed the `Text.Megaparsec.Char.Lexer.charLiteral` so it can accept longer
  escape sequences (max length is now 10).

* Added the `binDigitChar` functions in `Text.Megaparsec.Byte` and
  `Text.Megaparsec.Char`.

* Added the `binary` functions in `Text.Megaparsec.Byte.Lexer` and
  `Text.Megaparsec.Char.Lexer`.

* Improved case-insensitive character matching in the cases when e.g.
  `isLower` and `isUpper` both return `False`. Functions affected:
  `Text.Megaparsec.Char.char'`.

* Renamed `getPosition` to `getSourcePos`.

* Renamed `getTokensProcessed` to `getOffset`, `setTokensProcessed` to
  `setOffset`.

* Dropped `getTabWidth` and `setTabWidth` because tab width is irrelevant to
  parsing process now, it's only relevant for pretty-printing of parse
  errors, which is handled separately.

* Added and `withParsecT` in `Text.Megaparsec.Internal` to allow changing
  the type of the custom data component in parse errors.

### Parser state and input stream

* Dropped stacks of source positions. Accordingly, the functions
  `pushPosition` and `popPosition` from `Text.Megaparsec` and
  `sourcePosStackPretty` from `Text.Megaparsec.Error` were removed. The
  reason for this simplification is that I could not find any code that uses
  the feature and it makes manipulation of source positions hairy.

* Introduced `PosState` for calculating `SourcePos` from offsets and getting
  offending line for displaying on pretty-printing of parse errors. It's now
  contained in both `State` and `ParseErrorBundle`.

* Dropped `positionAt1`, `positionAtN`, `advance1`, and `advanceN` methods
  from `Stream`. They are no longer necessary because `reachOffset` (and its
  specialized version `reachOffsetNoLine`) takes care of `SourcePos`
  calculation.

### Parse errors

* `ParseError` now contains raw offset in input stream instead of
  `SourcePos`. `errorPos` was dropped from `Text.Megaparsec.Error`.

* `ParseError` is now parametrized over stream type `s` instead of token
  type `t`.

* Introduced `ParseErrorBundle` which contains one or more `ParseError`
  equipped with all information that is necessary to pretty-print them
  together with offending lines from the input stream. Functions like
  `runParser` now return `ParseErrorBundle` instead of plain `ParseError`.

  By default there will be only one `ParseError` in such a bundle, but it's
  possible to add more parse errors to a bundle manually. During
  pretty-printing, the input stream will be traversed only once.

* The primary function for pretty-printing of parse
  errors—`errorBundlePretty` always prints offending lines now.
  `parseErrorPretty` is still there, but it probably won't see a lot of use
  from now on. `parseErrorPretty'` and `parseErrorPretty_` were removed.
  `parseTest'` was removed because `parseTest` always prints offending lines
  now.

* Added `attachSourcePos` function in `Text.Megaparsec.Error`.

* The `ShowToken` type class has been removed and its method `showTokens`
  now lives in the `Stream` type class.

* The `LineToken` type class is no longer necessary because the new method
  `reachOffset` of the type class `Stream` does its job.

* In `Text.Megaparsec.Error` the following functions were added:
  `mapParseError`, `errorOffset`.

* Implemented continuous highlighting in parse errors. For this we added the
  `errorComponentLen` method to the `ShowErrorComponent` type class.

### Parse error builder

* The functions `err` and `errFancy` now accept offsets at which the parse
  errors are expected to have happened, i.e. `Int`s. Thus `posI` and `posN`
  are no longer necessary and were removed.

* `ET` is now parametrized over the type of stream `s` instead of token type
  `t`.

* Combinators like `utoks` and `etoks` now accept chunks of input stream
  directly, i.e. `Tokens s` instead of `[Token s]` which should be more
  natural and convenient.

## Megaparsec 6.5.0

* Added `Text.Megaparsec.Internal`, which exposes some internal data
  structures and data constructor of `ParsecT`.

## Megaparsec 6.4.1

* `scientific` now correctly backtracks after attempting to parse fractional
  and exponent parts of a number. `float` correctly backtracks after
  attempting to parse optional exponent part (when it comes after fractional
  part, otherwise it's obligatory).

## Megaparsec 6.4.0

* `Text.Megaparsec` now re-exports `Control.Monad.Combinators` instead of
  `Control.Applicative.Combinators` from `parser-combinators` because the
  monadic counterparts of the familiar combinators are more efficient and
  not as leaky.

  This may cause minor breakage in certain cases:

  * You import `Control.Applicative` and in that case there will be a name
    conflict between `Control.Applicative.many` and
    `Control.Monad.Combinator.many` now (the same for `some`).

  * You define a polymorphic helper in terms of combinator(s) from
    `Control.Applicative.Combinators` and use `Applicative` or `Alternative`
    constraint. In this case you'll have to adjust the constraint to be
    `Monad` or `MonadPlus` respectively.

  Also note that the new `Control.Monad.Combinators` module we re-export now
  re-exports `empty` from `Control.Applicative`.

* Fix the `atEnd` parser. It now does not produce hints, so when you use it,
  it won't contribute to the “expecting end of input” component of parse
  error.

## Megaparsec 6.3.0

* Added an `IsString` instance for `ParsecT`. Now it is possible to
  write `"abc"` rather than `string "abc"`.

* Added the `customFailure` combinator, which is a special case of
  `fancyFailure`.

* Made implementation of `sconcat` and `mconcat` of `ParsecT` more
  efficient.

## Megaparsec 6.2.0

* `float` in `Text.Megaparsec.Char.Lexer` and `Text.Megaparsec.Byte.Lexer`
  now does not accept plain integers. This is the behavior we had in version
  5 of the library.

## Megaparsec 6.1.1

* Fixed the bug when `tokens` used `cok` continuation even when matching an
  empty chunk. Now it correctly uses `eok` in this case.

## Megaparsec 6.1.0

* Improved rendering of offending line in `parseErrorPretty'` in the
  presence of tab characters.

* Added `parseErrorPretty_`, which is just like `parseErrorPretty'` but
  allows to specify tab width to use.

* Adjusted hint generation so when we backtrack a consuming parser with
  `try`, we do not create hints from its parse error (because it's further
  in input stream!). This was a quite subtle bug that stayed unnoticed for
  several years apparently.

## Megaparsec 6.0.2

* Allow `parser-combinators-0.2.0`.

## Megaparsec 6.0.1

* Fixed a typo in `README.md`.

* Added some text that clarifies how to parametrize the `ParseError` type.

## Megaparsec 6.0.0

### General

* Re-organized the module hierarchy. Some modules such as
  `Text.Megaparsec.Prim` do not exist anymore. Stream definitions were moved
  to `Text.Megaparsec.Stream`. Generic combinators are now re-exported from
  the `Control.Applicative.Combinators` from the package
  `parser-combinators`. Just import `Text.Megaparsec` and you should be OK.
  Add `Text.Megaparsec.Char` if you are working with a stream of `Char`s or
  `Text.Megaparsec.Byte` if you intend to parse binary data, then add
  qualified modules you need (permutation parsing, lexing, expression
  parsing, etc.). `Text.Megaparsec.Lexer` was renamed to
  `Text.Megaparsec.Char.Lexer` because many functions in it has the `Token s
  ~ Char` constraint. There is also `Text.Megaparsec.Byte.Lexer` now,
  although it has fewer functions.

* Dropped per-stream modules, the `Parser` type synonym is to be defined
  manually by user.

* Added a `MonadFix` instance for `ParsecT`.

* More lightweight dependency tree, dropped `exceptions` and `QuickCheck`
  dependencies.

* Added dependency on `case-insensitive`.

### Source positions

* Now `Pos` contains an `Int` inside, not `Word`.

* Dropped `unsafePos` and changed type of `mkPos` so it throws from pure
  code if its argument is not a positive `Int`.

* Added `pos1` constant that represents the `Pos` with value 1 inside.

* Made `InvalidPosException` contain the invalid `Int` value that was passed
  to `mkPos`.

### Parse errors

* Changed the definition of `ParseError` to have separate data constructors
  for “trivial” errors (unexpected/expected tokens) and “fancy” errors
  (everything else).

* Removed the `ErrorComponent` type class, added `ErrorFancy` instead.
  `ErrorFancy` is a sum type which can represent `fail` messages, incorrect
  indentation, and custom data (we use `Void` for that by default to
  “disable” it). This is better than the typeclass-based approach because
  every instance of `ErrorComponent` needed to have constructors for `fail`
  and indentation massages anyway, leading to duplication of code (for
  example for parse error component rendering).

* Added `Functor` instances for `ErrorItem` and `ErrorFancy`.

* Added the function `errorPos` to get error positions from `ParseError`
  (previously it was a record selector in `ParseError`).

* Control characters in parse error are displayed in a readable form even
  when they are part of strings, for example: `{<newline>` (`{` followed by
  the newline character). Previously control characters were rendered in
  readable form only as standalone tokens.

* Added `Text.Megaparsec.Error.Builder` module to help construct
  `ParseError`s easily. It is useful for testing and debugging. Previously
  we had something like that in the `hspec-megaparsec` package, but it does
  not hurt to ship it with the library.

* Added `parseErrorPretty'` allowing to display offending line in parse
  errors.

* Added `LineToken` type class for tokens that support operations necessary
  for selecting and displaying relevant line of input (used in
  `parseErrorPretty'`).

* Added `parseTest'` function that is just like `parseTest`, but also prints
  offending line in parse errors. This is powered by the new
  `parseErrorPretty'`.

### Stream

* Introduced the new `Text.Megaparsec.Stream` module that is the home of
  `Stream` type class. In version 6, the type class has been extended
  significantly to improve performance and make some combinators more
  general.

### Combinators

* Changed signatures of `failure` and `token`, they only can signal trivial
  errors now.

* Added a new method of `MonadParsec` type class called `fancyFailure` for
  signalling non-trivial failures. Signatures of some functions (`failure`,
  `token`) have been changed accordingly.

* Added `takeWhileP`, `takeWhile1P` and `takeP` to `MonadParsec`.

* Added `takeRest` non-primitive combinator to consume the rest of input.

* Added `atEnd` which returns `True` when end of input has been reached.

* Dropped `oneOf'` and `noneOf'` from `Text.Megaparsec.Char`. These were
  seldom (if ever) used and are easily re-implemented.

* Added `notChar` in `Text.Megaparsec.Char`.

* Added `space1` in `Text.Megaparsec.Char`. This parser is like `space` but
  requires at least one space character to be present to succeed.

* Added new module `Text.Megaparsec.Byte`, which is similar to
  `Text.Megaparsec.Char`, but for token streams of the type `Word8` instead
  of `Char`.

* `integer` was dropped from `Text.Megaparsec.Char.Lexer`. Use `decimal`
  instead.

* `number` was dropped from `Text.Megaparsec.Char.Lexer`. Use `scientific`
  instead.

* `decimal`, `octal`, and `hexadecimal` are now polymorphic in their return
  type and can be used to parse any instance of `Integral`.

* `float` is now polymorphic in its return type and can be used to parse any
  instance of `RealFloat`.

* Added new module `Text.Megaparsec.Byte.Lexer`, which provides some
  functions (white space and numeric helpers) from
  `Text.Megaparsec.Char.Lexer` for streams with token type `Word8`.

## Megaparsec 5.3.1

* Various updates to the docs.

* Allowed `QuickCheck-2.10`.

## Megaparsec 5.3.0

* Added the `match` combinator that allows to get collection of consumed
  tokens along with result of parsing.

* Added the `region` combinator which allows to process parse errors
  happening when its argument parser is run.

* Added the `getNextTokenPosition`, which returns position where the next
  token in the stream begins.

* Defined `Semigroup` and `Monoid` instances of `ParsecT`.

* Dropped support for GHC 7.6.

* Added an `ErrorComponent` instance for `()`.

## Megaparsec 5.2.0

* Added `MonadParsec` instance for `RWST`.

* Allowed `many` to run parsers that do not consume input. Previously this
  signalled an `error` which was ugly. Of course, in most cases giving
  `many` a parser that do not consume input will lead to non-termination
  bugs, but there are legal cases when this should be allowed. The test
  suite now contains an example of this. Non-termination issues is something
  inherited from the power Megaparsec gives (with more power comes more
  responsibility), so that `error` case in `many` really does not solve the
  problem, it was just a little ah-hoc guard we got from Parsec's past.

* The criterion benchmark was completely re-written and a new weigh
  benchmark to analyze memory consumption was added.

* Performance improvements: `count` (marginal improvement, simpler
  implementation), `count'` (considerable improvement), and `many`
  (marginal improvement, simpler implementation).

* Added `stateTokensProcessed` field to parser state and helper functions
  `getTokensProcessed` and `setTokensProcessed`. The field contains number
  of processed tokens so far. This allows, for example, create wrappers that
  return just parsed fragment of input stream alongside with result of
  parsing. (It was possible before, but very inefficient because it required
  traversing entire input stream twice.)

* `IndentNone` option of `indentBlock` now picks whitespace after it like
  its sisters `IndentMany` and `IndentSome` do, see #161.

* Fixed a couple of quite subtle bugs in `indentBlock` introduced by
  changing behaviour of `skipLineComment` in version 5.1.0. See #178 for
  more information.

## Megaparsec 5.1.2

* Stopped using property tests with `dbg` helper to avoid flood of debugging
  info when test suite is run.

* Fixed the build with `QuickCheck` versions older than 2.9.0.

## Megaparsec 5.1.1

* Exported the `observing` primitive from `Text.Megaparsec`.

## Megaparsec 5.1.0

* Defined `displayException` for `ParseError`, so exceptions are displayed
  in human-friendly form now. This works with GHC 7.10 and later.

* Line comments parsed by `skipLineComment` now may end at the end of input
  and do not necessarily require a newline to be parsed correctly. See #119.

* Exposed `parseErrorTextPretty` function in `Text.Megaparsec.Error` to
  allow to render `ParseError`s without stack of source positions.

* Eliminated the `old-tests` test suite — Parsec legacy. The cases that are
  not already *obviously* covered in the main test suite were included into
  it.

* Added `Arbitrary` instances for the following data types: `Pos`,
  `SourcePos`, `ErrorItem`, `Dec`, `ParseError` and `State`. This should
  make testing easier without the need to add orphan instances every time.
  The drawback is that we start to depend on `QuickCheck`, but that's a fair
  price.

* The test suite now uses the combination of Hspec and the
  `hpesc-megaparsec` package, which also improved the latter (that package
  is the recommended way to test Megaparsec parsers).

* The `try` combinator now truly backtracks parser state when its argument
  parser fails (either consuming input or not). Most users will never notice
  the difference though. See #142.

* Added the `dbg` function that should be helpful for debugging.

* Added `observing` primitive combinator that allows to “observe” parse
  errors without ending parsing (they are returned in `Left`, while normal
  results are wrapped in `Right`).

* Further documentation improvements.

## Megaparsec 5.0.1

* Derived `NFData` instances for `Pos`, `InvalidPosException`, `SourcePos`,
  `ErrorItem`, `Dec`, `ParseError`, and `State`.

* Derived `Data` instance for `ParseError`, `Data` and `Typeable` instances
  for `SourcePos` and `State`.

* Minor documentation improvements.

## Megaparsec 5.0.0

### General changes

* Removed `parseFromFile` and `StorableStream` type-class that was necessary
  for it. The reason for removal is that reading from file and then parsing
  its contents is trivial for every instance of `Stream` and this function
  provides no way to use newer methods for running a parser, such as
  `runParser'`. So, simply put, it adds little value and was included in 4.x
  versions for compatibility reasons.

* Moved position-advancing function from arguments of `token` and `tokens`
  functions to `Stream` type class (named `updatePos`). The new function
  allows to handle custom streams of tokens where every token contains
  information about its position in stream better (for example when stream
  of tokens is produced with happy/alex).

* Support for include files (stack of positions instead of flat position)
  added. The new functions `pushPosition` and `popPosition` can be used to
  move “vertically” in the stack of positions. `getPosition` and
  `setPosition` still work on top (“current file”) level, but user can get
  full stack via `getParserState` if necessary. Note that `ParseError` and
  pretty-printing for it also support the new feature.

* Added type function `Token` associated with `Stream` type class. The
  function returns type of token corresponding to specific token stream.

* Type `ParsecT` (and also type synonym `Parsec`) are now parametrized over
  type of custom component in parse errors.

* Parameters of `MonadParsec` type class are: `e` — type of custom component
  in parse errors, `s` — type of input stream, and `m` — type of underlying
  monad.

* Type of `failure` primitive combinator was changed, now it accepts three
  arguments: set of unexpected items, set of expected items, and set of
  custom data.

* Type of `token` primitive combinator was changed, now in case of failure a
  triple-tuple is returned with elements corresponding to arguments of
  `failure` primitive. The `token` primitive can also be optionally given an
  argument of token type to use in error messages (as expected item) in case
  of end of input.

* `unexpected` combinator now accepts argument of type `ErrorItem` instead
  of plain `String`.

* General performance improvements and improvements in speed of some
  combinators, `manyTill` in particular.

### Error messages

* The module `Text.Megaparsec.Pos` was completely rewritten. The new module
  uses `Pos` data type with smart constructors to ensure that things like
  line and column number can be only positive. `SourcePos` on the other hand
  does not require smart constructors anymore and its constructors are
  exported. `Show` and `Read` instances of `SourcePos` are derived and
  pretty-printing is done with help of `sourcePosPretty` function.

* The module `Text.Megaparsec.Error` was completely rewritten. A number of
  new types and type-classes are introduced: `ErrorItem`, `Dec`,
  `ErrorComponent`, and `ShowErrorComponent`. `ParseError` does not need
  smart constructors anymore and its constructor and field selectors are
  exported. It uses sets (from the `containers` package) instead of sorted
  lists to enumerate unexpected and expected items. The new definition is
  also parametrized over token type and custom data type which can be passed
  around as part of parse error. Default “custom data” component is `Dec`,
  which see. All in all, we have completely well-typed and extensible error
  messages now. `Show` and `Read` instances of `ParseError` are derived and
  pretty-printing is done with help of `parseErrorPretty`.

* The module `Text.Megaparsec.ShowToken` was eliminated and type class
  `ShowToken` was moved to `Text.Megaparsec.Error`. The only method of that
  class in now named `showTokens` and it works on streams of tokens, where
  single tokes are represented by `NonEmpty` list with single element.

### Built-in combinators

* Combinators `oneOf`, `oneOf'`, `noneOf`, and `noneOf'` now accept any
  instance of `Foldable`, not only `String`.

### Lexer

* Error messages about incorrect indentation levels were greatly improved.
  Now every such message contains information about desired ordering between
  “reference” indentation level and actual indentation level as well as
  values of these levels. The information is stored in `ParseError` in
  well-typed form and can be pretty-printed when necessary. As part of this
  improvement, type of `indentGuard` was changed.

* `incorrectIndent` combinator is introduced in `Text.Megaparsec.Lexer`
  module. It allows to fail with detailed information regarding incorrect
  indentation.

* Introduced `scientific` parser that can parse arbitrary big numbers
  without error or memory overflow. `float` still returns `Double`, but it's
  defined in terms of `scientific` now. Since `Scientific` type can reliably
  represent integer values as well as floating point values, `number` now
  returns `Scientific` instead of `Either Integer Double` (`Integer` or
  `Double` can be extracted from `Scientific` value anyway). This in turn
  makes `signed` parser more natural and general, because we do not need
  ad-hoc `Signed` type class anymore.

* Added `skipBlockCommentNested` function that should help parse possibly
  nested block comments.

* Added `lineFold` function that helps parse line folds.

## Megaparsec 4.4.0

* Now state returned on failure is the exact state of parser at the moment
  when it failed, which makes incremental parsing feature much better and
  opens possibilities for features like “on-the-fly” recovering from parse
  errors.

* The `count` combinator now works with `Applicative` instances (previously
  it worked only with instances of `Alternative`). It's now also faster.

* `tokens` and parsers built upon it (such as `string` and `string'`)
  backtrack automatically on failure now, that is, when they fail, they
  never consume any input. This is done to make their consumption model
  match how error messages are reported (which becomes an important thing as
  user gets more control with primitives like `withRecovery`). This means,
  in particular, that it's no longer necessary to use `try` with
  `tokens`-based parsers. This new feature *does not* affect performance in
  any way.

* New primitive parser `withRecovery` added. The parser allows to recover
  from parse errors “on-the-fly” and continue parsing. Once parsing is
  finished, several parse errors may be reported or ignored altogether.

* `eitherP` combinator added.

* Removed `Enum` instance of `Message` type. This was Parsec's legacy that
  we should eliminate now. `Message` does not constitute enumeration,
  `toEnum` was never properly defined for it. The idea to use `fromEnum` to
  determine type of `Message` is also ugly, for this purpose new functions
  `isUnexpected`, `isExpected`, and `isMessage` are defined in
  `Text.Megaparsec.Error`.

* Minor tweak in signature of `MonadParsec` type class. Collection of
  constraints changed from `Alternative m, Monad m, Stream s t` to
  `Alternative m, MonadPlus m, Stream s t`. This is done to make it easier
  to write more abstract code with older GHC where such primitives as
  `guard` are defined for instances of `MonadPlus`, not `Alternative`.

## Megaparsec 4.3.0

* Canonicalized `Applicative`/`Monad` instances. Thanks to Herbert Valerio
  Riedel.

* Custom messages in `ParseError` are printed each on its own line.

* Now accumulated hints are not used with `ParseError` records that have
  only custom messages in them (created with `Message` constructor, as
  opposed to `Unexpected` or `Expected`). This strips “expected” line from
  custom error messages where it's unlikely to be relevant anyway.

* Added higher-level combinators for indentation-sensitive grammars:
  `indentLevel`, `nonIndented`, and `indentBlock`.

## Megaparsec 4.2.0

* Made `newPos` constructor and other functions in `Text.Megaparsec.Pos`
  smarter. Now it's impossible to create `SourcePos` with non-positive line
  number or column number. Unfortunately we cannot use `Numeric.Natural`
  because we need to support older versions of `base`.

* `ParseError` is now a monoid. `mergeError` is used as `mappend`.

* Added functions `addErrorMessages` and `newErrorMessages` to add several
  messages to existing error and to construct error with several attached
  messages respectively.

* `parseFromFile` now lives in `Text.Megaparsec.Prim`. Previously we had 5
  nearly identical definitions of the function, varying only in
  type-specific `readFile` function. Now the problem is solved by
  introduction of `StorableStream` type class. All supported stream types
  are instances of the class out of box and thus we have polymorphic version
  of `parseFromFile`.

* `ParseError` is now instance of `Exception` (and `Typeable`).

* Introduced `runParser'` and `runParserT'` functions that take and return
  parser state. This makes it possible to partially parse input, resume
  parsing, specify non-standard initial textual position, etc.

* Introduced `failure` function that allows to fail with arbitrary
  collection of messages. `unexpected` is now defined in terms of
  `failure`. One consequence of this design decision is that `failure` is
  now method of `MonadParsec`, while `unexpected` is not.

* Removed deprecated combinators from `Text.Megaparsec.Combinator`:

    * `chainl`
    * `chainl1`
    * `chainr`
    * `chainr1`

* `number` parser in `Text.Megaparsec.Lexer` now can be used with `signed`
  combinator to parse either signed `Integer` or signed `Double`.

## Megaparsec 4.1.1

* Fixed bug in implementation of `sepEndBy` and `sepEndBy1` and removed
  deprecation notes for these functions.

* Added tests for `sepEndBy` and `sepEndBy1`.

## Megaparsec 4.1.0

* Relaxed dependency on `base`, so that minimal required version of `base`
  is now 4.6.0.0. This allows Megaparsec to compile with GHC 7.6.x.

* `Text.Megaparsec` and `Text.Megaparsec.Prim` do not export data types
  `Consumed` and `Reply` anymore because they are rather low-level
  implementation details that should not be visible to end-user.

* Representation of file name and textual position in error messages was
  made conventional.

* Fixed some typos is documentation and other materials.

## Megaparsec 4.0.0

### General changes

* Renamed `many1` → `some` as well as other parsers that had `many1` part in
  their names.

* The following functions are now re-exported from `Control.Applicative`:
  `(<|>)`, `many`, `some`, `optional`. See #9.

* Introduced type class `MonadParsec` in the style of MTL monad
  transformers. Eliminated built-in user state since it was not flexible
  enough and can be emulated via stack of monads. Now all tools in
  Megaparsec work with any instance of `MonadParsec`, not only with
  `ParsecT`.

* Added new function `parseMaybe` for lightweight parsing where error
  messages (and thus file name) are not important and entire input should be
  parsed. For example it can be used when parsing of single number according
  to specification of its format is desired.

* Fixed bug with `notFollowedBy` always succeeded with parsers that don't
  consume input, see #6.

* Flipped order of arguments in the primitive combinator `label`, see #21.

* Renamed `tokenPrim` → `token`, removed old `token`, because `tokenPrim` is
  more general and original `token` is little used.

* Made `token` parser more powerful, now its second argument can return
  `Either [Message] a` instead of `Maybe a`, so it can influence error
  message when parsing of token fails. See #29.

* Added new primitive combinator `hidden p` which hides “expected” tokens in
  error message when parser `p` fails.

* Tab width is not hard-coded anymore. It can be manipulated via
  `getTabWidth` and `setTabWidth`. Default tab-width is `defaultTabWidth`,
  which is 8.

### Error messages

* Introduced type class `ShowToken` and improved representation of
  characters and strings in error messages, see #12.

* Greatly improved quality of error messages. Fixed entire
  `Text.Megaparsec.Error` module, see #14 for more information. Made
  possible normal analysis of error messages without “render and re-parse”
  approach that previous maintainers had to practice to write even simplest
  tests, see module `Utils.hs` in `old-tests` for example.

* Reduced number of `Message` constructors (now there are only `Unexpected`,
  `Expected`, and `Message`). Empty “magic” message strings are ignored now,
  all the library now uses explicit error messages.

* Introduced hint system that greatly improves quality of error messages and
  made code of `Text.Megaparsec.Prim` a lot clearer.

### Built-in combinators

* All built-in combinators in `Text.Megaparsec.Combinator` now work with any
  instance of `Alternative` (some of them even with `Applicative`).

* Added more powerful `count'` parser. This parser can be told to parse from
  `m` to `n` occurrences of some thing. `count` is defined in terms of
  `count'`.

* Removed `optionMaybe` parser, because `optional` from
  `Control.Applicative` does the same thing.

* Added combinator `someTill`.

* These combinators are considered deprecated and will be removed in future:

    * `chainl`
    * `chainl1`
    * `chainr`
    * `chainr1`
    * `sepEndBy`
    * `sepEndBy1`

### Character parsing

* Renamed some parsers:

    * `alphaNum` → `alphaNumChar`
    * `digit` → `digitChar`
    * `endOfLine` → `eol`
    * `hexDigit` → `hexDigitChar`
    * `letter` → `letterChar`
    * `lower` → `lowerChar`
    * `octDigit` → `octDigitChar`
    * `space` → `spaceChar`
    * `spaces` → `space`
    * `upper` → `upperChar`

* Added new character parsers in `Text.Megaparsec.Char`:

    * `asciiChar`
    * `charCategory`
    * `controlChar`
    * `latin1Char`
    * `markChar`
    * `numberChar`
    * `printChar`
    * `punctuationChar`
    * `separatorChar`
    * `symbolChar`

* Descriptions of old parsers have been updated to accent some
  Unicode-specific moments. For example, old description of `letter` stated
  that it parses letters from “a” to “z” and from “A” to “Z”. This is wrong,
  since it used `Data.Char.isAlpha` predicate internally and thus parsed
  many more characters (letters of non-Latin languages, for example).

* Added combinators `char'`, `oneOf'`, `noneOf'`, and `string'` which are
  case-insensitive variants of `char`, `oneOf`, `noneOf`, and `string`
  respectively.

### Lexer

* Rewritten parsing of numbers, fixed #2 and #3 (in old Parsec project these
  are number 35 and 39 respectively), added per bug tests.

    * Since Haskell report doesn't say anything about sign, `integer` and
      `float` now parse numbers without sign.

    * Removed `natural` parser, it's equal to new `integer` now.

    * Renamed `naturalOrFloat` → `number` — this doesn't parse sign too.

    * Added new combinator `signed` to parse all sorts of signed numbers.

* Transformed `Text.Parsec.Token` into `Text.Megaparsec.Lexer`. Little of
  Parsec's code remains in the new lexer module. New module doesn't impose
  any assumptions on user and should be vastly more useful and
  general. Hairy stuff from original Parsec didn't get here, for example
  built-in Haskell functions are used to parse escape sequences and the like
  instead of trying to re-implement the whole thing.

### Other

* Renamed the following functions:

    * `permute` → `makePermParser`
    * `buildExpressionParser` → `makeExprParser`

* Added comprehensive QuickCheck test suite.

* Added benchmarks.

## Parsec 3.1.9

* Many and various updates to documentation and package description
  (including the homepage links).

* Add an `Eq` instance for `ParseError`.

* Fixed a regression from 3.1.6: `runP` is again exported from module
  `Text.Parsec`.

## Parsec 3.1.8

* Fix a regression from 3.1.6 related to exports from the main module.

## Parsec 3.1.7

* Fix a regression from 3.1.6 related to the reported position of error
  messages. See bug #9 for details.

* Reset the current error position on success of `lookAhead`.

## Parsec 3.1.6

* Export `Text` instances from `Text.Parsec`.

* Make `Text.Parsec` exports more visible.

* Re-arrange `Text.Parsec` exports.

* Add functions `crlf` and `endOfLine` to `Text.Parsec.Char` for handling
  input streams that do not have normalized line terminators.

* Fix off-by-one error in `Token.charControl`.

## Parsec 3.1.4 & 3.1.5

* Bump dependency on `text`.

## Parsec 3.1.3

* Fix a regression introduced in 3.1.2 related to positions reported by
  error messages.

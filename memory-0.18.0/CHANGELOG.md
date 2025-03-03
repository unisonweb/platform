## 0.18

* drop support for ghc < 8.8
* compat with ghc 9.4

## ...

## 0.14.18

* Branch/Release Snafu

## 0.14.17

* Require basement >= 0.0.7, Fix compilation with GHC 8,6
* Cleanup CPP, dropping support for much older version

## 0.14.16

* Fix compilation with a newer basement (>= 0.0.7) and an older GHC (< 8.0)

## 0.14.15

* Convert tests to foundation checks
* Convert CI to haskell-ci
* Fix compilation without foundation
* Introduce ByteArrayL and associated method, as a type level sized version of ByteArray
* Add NormalForm for Bytes and ScrubbedBytes

## 0.14.14

* Fix bounds issues with empty strings in base64 and base32
* Improve tests compatibility w.r.t old basement version

## 0.14.13

* Handle compat SPECIALIZE for older GHC

## 0.14.12

* Optimise copy operations and convert
* Add instance of ByteArrayAccess and ByteArray for Block
* Add Block and UArray in memory's tests

## 0.14.11

* Fix issue in unBase64 with an empty bytestring that would cause a segfault

## 0.14.10

* Reintroduce foundation compatibility with old version

## 0.14.9

* Reduce dependency to basement

## 0.14.8

* Fix incompatibility with foundation 0.0.14

## 0.14.7

* Fix typo in state passing

## 0.14.6

* Fix allocRet using unit of bytes but using as unit of ty directly without adaptation

## 0.14.5

* Fix bug in memXorWith not working as advertised if source different from destination

## 0.14.4

* Add support for foundation uarray creation
* optimise memXorWith

## 0.14.3

* Add support for foundation uarray peeking

## 0.14.2

* Fix use of ghc 8.2 touch
* Prevent span from reading past buffer
* cleanup .prof spam

## 0.14.1

* Fix `Show` instance of Bytes (Oliver Chéron)

## 0.14

* Improve fromW64BE
* Add IsString instance for ScrubbedBytes

## 0.13

* Add combinator to check for end of parsing.

## 0.12

* Fix compilation with mkWeak and latest GHC (Lars Kuhtz)

## 0.11

* add support for GHC 8.0.1

## 0.10

* make memConstEqual more constant not using boolean comparaison

## 0.9

* memConstEqual was comparing length times the first byte instead of comparing all the bytes one to one

## 0.8

* Add Base64 variants (Luke Taylor)
* Fix compilation on Haiku (Jessica Hamilton)

## 0.7

* Fix fixed sized scrubber written too hastily, that would zero out memory, as the index
  was written through byte size, whereas the primitive would consider it as WordX type index.
  it would helps if Ghc.Prim had better documentation.

## 0.6

* Fix compilation on architecture where endianness is not a compile time define related
  to their cabal arch().

## 0.5

* Add Base32 support (Nicolas Di Prima)
* Fix build on 32 bit by simplifying scrubber, and adding Word64 type + operations compatibility

## 0.4

* Add Ord instances for SipHash and FnvHash (Nicolas Di Prima)
* Fix GHC-7.2 build by defining unsafeShiftL (Adam Bergmark)
* Fix show instance of Bytes to properly display each bytes instead of just the end one
* Add View type that emulate a view on a ByteArray type (Nicolas Di Prima)

## 0.3

* fix missing modules from tests on sdist

## 0.2

* make concat more generic as to what the output is going to be, and at the same
  time reduce the constraint on the input to just Access
* make all byte array operation safer related to negative size. now replicate, zero, and alloc will returns
  an empty byte array when asking for negative size
* replace 'pack' in Data.ByteArray.Pack by 'fill', as to not conflict with 'Data.ByteArray.pack'.
  Also swap the length and monadic action to be more naturally used
* add a deprecated 'pack' that alias to 'fill' for now
* loosen constraint of Data.ByteArray.Pack.putBytes from ByteArray to ByteArrayAccess

## 0.1

* Initial release

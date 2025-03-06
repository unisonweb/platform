## 0.34

* Hashing getRandomBytes before using as Seed for ChaChaDRG
  [#24](https://github.com/kazu-yamamoto/crypton/pull/24)
* Add support for XChaCha and XChaChaPoly1305
  [#18](https://github.com/kazu-yamamoto/crypton/pull/18)
* Strict byteArray of IV c
  [#16](https://github.com/kazu-yamamoto/crypton/pull/16)

## 0.33

* Add "crypton_" prefix to the final C symbols.
  [#9](https://github.com/kazu-yamamoto/crypton/pull/9)

## 0.32

* All C symbols now have the "crypton_" prefix.
  [#7](https://github.com/kazu-yamamoto/crypton/pull/7)
  [#8](https://github.com/kazu-yamamoto/crypton/pull/8)

## 0.31

* Crypton is forked from cryptonite with the original authors permission.
* Ignoring exceptons from hClose to read the next entropy
  [#1](https://github.com/kazu-yamamoto/crypton/pull/1)
* Enabling the support_pclmuldq flag by default.

## 0.30

* Fix some C symbol blake2b prefix to be cryptonite_ prefix (fix mixing with other C library)
* add hmac-lazy
* Fix compilation with GHC 9.2
* Drop support for GHC8.0, GHC8.2, GHC8.4, GHC8.6

## 0.29

* advance compilation with gmp breakage due to change upstream
* Add native EdDSA support

## 0.28

* Add hash constant time capability
* Prevent possible overflow during hashing by hashing in 4GB chunks

## 0.27

* Optimise AES GCM and CCM
* Optimise P256R1 implementation
* Various AES-NI building improvements
* Add better ECDSA support
* Add XSalsa derive
* Implement square roots for ECC binary curve
* Various tests and benchmarks

## 0.26

* Add Rabin cryptosystem (and variants)
* Add bcrypt_pbkdf key derivation function
* Optimize Blowfish implementation
* Add KMAC (Keccak Message Authentication Code)
* Add ECDSA sign/verify digest APIs
* Hash algorithms with runtime output length
* Update blake2 to latest upstream version
* RSA-PSS with arbitrary key size
* SHAKE with output length not divisible by 8
* Add Read and Data instances for Digest type
* Improve P256 scalar primitives
* Fix hash truncation bug in DSA
* Fix cost parsing for bcrypt
* Fix ECC failures on arm64
* Correction to PKCS#1 v1.5 padding
* Use powModSecInteger when available
* Drop GHC 7.8 and GHC 7.10 support, refer to pkg-guidelines
* Optimise GCM mode
* Add little endian serialization of integer

## 0.25

* Improve digest binary conversion efficiency
* AES CCM support
* Add MonadFailure instance for CryptoFailable
* Various misc improvements on documentation
* Edwards25519 lowlevel arithmetic support
* P256 add point negation
* Improvement in ECC (benchmark, better normalization)
* Blake2 improvements to context size
* Use gauge instead of criterion
* Use haskell-ci for CI scripts
* Improve Digest memory representation to be 2 less Ints and one less boxing
  moving from `UArray` to `Block`

## 0.24

* Ed25519: generateSecret & Documentation updates
* Repair tutorial
* RSA: Allow signing digest directly
* IV add: fix overflow behavior
* P256: validate point when decoding
* Compilation fix with deepseq disabled
* Improve Curve448 and use decaf for Ed448
* Compilation flag blake2 sse merged in sse support
* Process unaligned data better in hashes and AES, on architecture needing alignment
* Drop support for ghc 7.6
* Add ability to create random generator Seed from binary data and
  loosen constraint on ChaChaDRG seed from ByteArray to ByteArrayAccess.
* Add 3 associated types with the HashAlgorithm class, to get
  access to the constant for BlockSize, DigestSize and ContextSize at the type level.
  the related function that this replaced will be deprecated in later release, and
  eventually removed.

API CHANGES:

* Improve ECDH safety to return failure for bad inputs (e.g. public point in small order subgroup).
  To go back to previous behavior you can replace `ecdh` by `ecdhRaw`. It's recommended to
  use `ecdh` and handle the error appropriately.
* Users defining their own HashAlgorithm needs to define the
  HashBlockSize, HashDigest, HashInternalContextSize associated types

## 0.23

* Digest memory usage improvement by using unpinned memory
* Fix generateBetween to generate within the right bounds
* Add pure Twofish implementation
* Fix memory allocation in P256 when using a temp point
* Consolidate hash benchmark code
* Add Nat-length Blake2 support (GHC > 8.0)
* Update tutorial

## 0.22

* Add Argon2 (Password Hashing Competition winner) hash function
* Update blake2 to latest upstream version
* Add extra blake2 hashing size
* Add faster PBKDF2 functions for SHA1/SHA256/SHA512
* Add SHAKE128 and SHAKE256
* Cleanup prime generation, and add tests
* Add Time-based One Time Password (TOTP) and HMAC-based One Time Password (HOTP)
* Rename Ed448 module name to Curve448, old module name still valid for now

## 0.21

* Drop automated tests with GHC 7.0, GHC 7.4, GHC 7.6. support dropped, but probably still working.
* Improve non-aligned support in C sources, ChaCha and SHA3 now probably work on arch without support for unaligned access. not complete or tested.
* Add another ECC framework that is more flexible, allowing different implementations to work instead of
  the existing Pure haskell NIST implementation.
* Add ECIES basic primitives
* Add XSalsa20 stream cipher
* Process partial buffer correctly with Poly1305

## 0.20

* Fixed hash truncation used in ECDSA signature & verification (Olivier Chéron)
* Fix ECDH when scalar and coordinate bit sizes differ (Olivier Chéron)
* Speed up ECDSA verification using Shamir's trick (Olivier Chéron)
* Fix rdrand on windows

## 0.19

* Add tutorial (Yann Esposito)
* Derive Show instance for better interaction with Show pretty printer (Eric Mertens)

## 0.18

* Re-used standard rdrand instructions instead of bytedump of rdrand instruction
* Improvement to F2m, including lots of tests (Andrew Lelechenko)
* Add error check on salt length in bcrypt

## 0.17

* Add Miyaguchi-Preneel construction (Kei Hibino)
* Fix buffer length in scrypt (Luke Taylor)
* build fixes for i686 and arm related to rdrand

## 0.16

* Fix basepoint for Ed448

* Enable 64-bit Curve25519 implementation

## 0.15

* Fix serialization of DH and ECDH

## 0.14

* Reduce size of SHA3 context instead of allocating all-size fit memory. save
  up to 72 bytes of memory per context for SHA3-512.
* Add a Seed capability to the main DRG, to be able to debug/reproduce randomized program
  where you would want to disable the randomness.
* Add support for Cipher-based Message Authentication Code (CMAC) (Kei Hibino)
* *CHANGE* Change the `SharedKey` for `Crypto.PubKey.DH` and `Crypto.PubKey.ECC.DH`,
  from an Integer newtype to a ScrubbedBytes newtype. Prevent mistake where the
  bytes representation is generated without the right padding (when needed).
* *CHANGE* Keep The field size in bits, in the `Params` in `Crypto.PubKey.DH`,
  moving from 2 elements to 3 elements in the structure.

## 0.13

* *SECURITY* Fix buffer overflow issue in SHA384, copying 16 extra bytes from
  the SHA512 context to the destination memory pointer leading to memory
  corruption, segfault. (Mikael Bung)

## 0.12

* Fix compilation issue with Ed448 on 32 bits machine.

## 0.11

* Truncate hashing correctly for DSA
* Add support for HKDF (RFC 5869)
* Add support for Ed448
* Extends support for Blake2s to 224 bits version.
* Compilation workaround for old distribution (RHEL 4.1)
* Compilation fix for AIX
* Compilation fix with AESNI and ghci compiling C source in a weird order.
* Fix example compilation, typo, and warning

## 0.10

* Add reference implementation of blake2 for non-SSE2 platform
* Add support\_blake2\_sse flag

## 0.9

* Quiet down unused module imports
* Move Curve25519 over to Crypto.Error instead of using Either String.
* Add documentation for ChaChaPoly1305
* Add missing documentation for various modules
* Add a way to create Poly1305 Auth tag.
* Added support for the BLAKE2 family of hash algorithms
* Fix endianness of incrementNonce function for ChaChaPoly1305

## 0.8

* Add support for ChaChaPoly1305 Nonce Increment (John Galt)
* Move repository to the haskell-crypto organisation

## 0.7

* Add PKCS5 / PKCS7 padding and unpadding methods
* Fix ChaChaPoly1305 Decryption
* Add support for BCrypt (Luke Taylor)

## 0.6

* Add ChaChaPoly1305 AE cipher
* Add instructions in README for building on old OSX
* Fix blocking /dev/random Andrey Sverdlichenko

## 0.5

* Fix all strays exports to all be under the cryptonite prefix.

## 0.4

* Add a System DRG that represent a referentially transparent of evaluated bytes
  while using lazy evaluation for future entropy values.

## 0.3

* Allow drgNew to run in any MonadRandom, providing cascading initialization
* Remove Crypto.PubKey.HashDescr in favor of just having the algorithm
  specified in PKCS15 RSA function.
* Fix documentation in cipher sub section (Luke Taylor)
* Cleanup AES dead functions (Luke Taylor)
* Fix Show instance of Digest to display without quotes similar to cryptohash
* Use scrubbed bytes instead of bytes for P256 scalar

## 0.2

* Fix P256 compilation and exactness, + add tests
* Add a raw memory number serialization capability (i2osp, os2ip)
* Improve tests for number serialization
* Improve tests for ECC arithmetics
* Add Ord instance for Digest (Nicolas Di Prima)
* Fix entropy compilation on windows 64 bits.

## 0.1

* Initial release

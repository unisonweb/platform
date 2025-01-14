# Hashing and HMAC builtins

Unison has cryptographic builtins for hashing and computing [HMACs](https://en.wikipedia.org/wiki/HMAC) (hash-based message authentication codes). This transcript shows their usage and has some test cases.

``` ucm
scratch/main> ls builtin.Bytes
```
Notice the `fromBase16` and `toBase16` functions. Here's some convenience functions for converting `Bytes` to and from base-16 `Text`.

## API overview

Here's a few usage examples:

``` unison
ex1 = fromHex "2947db"
        |> crypto.hashBytes Sha3_512
        |> hex

ex2 = fromHex "02f3ab"
        |> crypto.hashBytes Blake2b_256
        |> hex

mysecret : Bytes
mysecret = fromHex "237be2"

ex3 = fromHex "50d3ab"
        |> (crypto.hmacBytes Sha2_256 mysecret)
        |> hex

f x = x

ex4 = crypto.hash Sha2_256 f |> hex

ex5 = crypto.hmac Sha2_256 mysecret f |> hex

> ex1
> ex2
> ex3
> ex4
> ex5
```

And here's the full API:

``` ucm
scratch/main> find-in builtin.crypto
```

Note that the universal versions of `hash` and `hmac` are currently unimplemented and will bomb at runtime:

``` unison
> hash Sha3_256 (fromHex "3849238492")
```

## Hashing tests

Here are some test vectors (taken from [here](https://www.di-mgt.com.au/sha_testvectors.html) and [here](https://en.wikipedia.org/wiki/BLAKE_(hash_function))) for the various hashing algorithms:

``` unison :hide
ex alg input expected = checks [hashBytes alg (ascii input) == fromHex expected]

test> sha3_512.tests.ex1 =
  ex Sha3_512
    "abc"
    "b751850b1a57168a5693cd924b6b096e08f621827444f70d884f5d0240d2712e10e116e9192af3c91a7ec57647e3934057340b4cf408d5a56592f8274eec53f0"

test> sha3_512.tests.ex2 =
  ex Sha3_512
    ""
    "a69f73cca23a9ac5c8b567dc185a756e97c982164fe25859e0d1dcc1475c80a615b2123af1f5f94c11e3e9402c3ac558f500199d95b6d3e301758586281dcd26"

test> sha3_512.tests.ex3 =
  ex Sha3_512
    "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
    "04a371e84ecfb5b8b77cb48610fca8182dd457ce6f326a0fd3d7ec2f1e91636dee691fbe0c985302ba1b0d8dc78c086346b533b49c030d99a27daf1139d6e75e"

test> sha3_512.tests.ex4 =
  ex Sha3_512
    "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
    "afebb2ef542e6579c50cad06d2e578f9f8dd6881d7dc824d26360feebf18a4fa73e3261122948efcfd492e74e82e2189ed0fb440d187f382270cb455f21dd185"

test> sha3_256.tests.ex1 =
  ex Sha3_256
    "abc"
    "3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532"

test> sha3_256.tests.ex2 =
  ex Sha3_256
    ""
    "a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a"

test> sha3_256.tests.ex3 =
  ex Sha3_256
    "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
    "41c0dba2a9d6240849100376a8235e2c82e1b9998a999e21db32dd97496d3376"

test> sha3_256.tests.ex4 =
  ex Sha3_256
    "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
    "916f6061fe879741ca6469b43971dfdb28b1a32dc36cb3254e812be27aad1d18"

test> sha2_512.tests.ex1 =
  ex Sha2_512
    "abc"
    "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"

test> sha2_512.tests.ex2 =
  ex Sha2_512
    ""
    "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e"

test> sha2_512.tests.ex3 =
  ex Sha2_512
    "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
    "204a8fc6dda82f0a0ced7beb8e08a41657c16ef468b228a8279be331a703c33596fd15c13b1b07f9aa1d3bea57789ca031ad85c7a71dd70354ec631238ca3445"

test> sha2_512.tests.ex4 =
  ex Sha2_512
    "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
    "8e959b75dae313da8cf4f72814fc143f8f7779c6eb9f7fa17299aeadb6889018501d289e4900f7e4331b99dec4b5433ac7d329eeb6dd26545e96e55b874be909"

test> sha2_256.tests.ex1 =
  ex Sha2_256
    "abc"
    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"

test> sha2_256.tests.ex2 =
  ex Sha2_256
    ""
    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

test> sha2_256.tests.ex3 =
  ex Sha2_256
    "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
    "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"

test> sha2_256.tests.ex4 =
  ex Sha2_256
    "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
    "cf5b16a778af8380036ce59e7b0492370b249b11e8f07a51afac45037afee9d1"

test> sha1.tests.ex1 =
  ex Sha1
    "abc"
    "a9993e364706816aba3e25717850c26c9cd0d89d"

test> sha1.tests.ex2 =
  ex Sha1
    ""
    "da39a3ee5e6b4b0d3255bfef95601890afd80709"

test> sha1.tests.ex3 =
  ex Sha1
    "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
    "84983e441c3bd26ebaae4aa1f95129e5e54670f1"

test> sha1.tests.ex4 =
  ex Sha1
    "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
    "a49b2446a02c645bf419f995b67091253a04a259"

test> blake2s_256.tests.ex1 =
  ex Blake2s_256
    ""
    "69217a3079908094e11121d042354a7c1f55b6482ca1a51e1b250dfd1ed0eef9"

test> blake2b_512.tests.ex1 =
  ex Blake2b_512
    ""
    "786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce"

test> blake2b_512.tests.ex2 =
  ex Blake2b_512
    "The quick brown fox jumps over the lazy dog"
    "a8add4bdddfd93e4877d2746e62817b116364a1fa7bc148d95090bc7333b3673f82401cf7aa2e4cb1ecd90296e3f14cb5413f8ed77be73045b13914cdcd6a918"

test> blake2b_512.tests.ex3 =
  ex Blake2b_512
    "The quick brown fox jumps over the lazy dof"
    "ab6b007747d8068c02e25a6008db8a77c218d94f3b40d2291a7dc8a62090a744c082ea27af01521a102e42f480a31e9844053f456b4b41e8aa78bbe5c12957bb"

-- check that hashing positive numbers that fit in both Nat and
-- Int yields the same answer
test> crypto.hash.numTests =
        t n =
          i = Int.fromRepresentation n
          hash Blake2b_256 n == hash Blake2b_256 i
        checks (List.map t (range 0 20))
```

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> test
```

## HMAC tests

These test vectors are taken from [RFC 4231](https://tools.ietf.org/html/rfc4231#section-4.3).

``` unison
ex' alg secret msg expected = checks [hmacBytes alg (fromHex secret) (ascii msg) == fromHex expected]

test> hmac_sha2_256.tests.ex1 =
  ex' Sha2_256
    "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"
    "Hi There"
    "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"
test> hmac_sha2_512.tests.ex1 =
  ex' Sha2_512
    "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"
    "Hi There"
    "87aa7cdea5ef619d4ff0b4241a1d6cb02379f4e2ce4ec2787ad0b30545e17cdedaa833b7d6b8a702038b274eaea3f4e4be9d914eeb61f1702e696c203a126854"

test> hmac_sha2_256.tests.ex2 =
  ex' Sha2_256
    "4a656665"
    "what do ya want for nothing?"
    "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"

test> hmac_sha2_512.tests.ex2 =
  ex' Sha2_512
    "4a656665"
    "what do ya want for nothing?"
    "164b7a7bfcf819e2e395fbe73b56e0a387bd64222e831fd610270cd7ea2505549758bf75c05a994a6d034f65f8f0e6fdcaeab1a34d4a6b4b636e070a38bce737"
```

## MD5 tests

Test vectors here pulled from [Wikipedia's writeup](https://en.wikipedia.org/wiki/MD5).

``` unison
ex alg input expected = checks [hashBytes alg (ascii input) == fromHex expected]

test> md5.tests.ex1 =
  ex Md5
    ""
    "d41d8cd98f00b204e9800998ecf8427e"

test> md5.tests.ex2 =
  ex Md5
    "The quick brown fox jumps over the lazy dog"
    "9e107d9d372bb6826bd81d3542a419d6"

test> md5.tests.ex3 =
  ex Md5
    "The quick brown fox jumps over the lazy dog."
    "e4d909c290d0fb1ca068ffaddf22cbd0"
```

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> test
```

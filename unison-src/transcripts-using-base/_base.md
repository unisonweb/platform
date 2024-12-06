# Base transcript

## Overview

This transcript is meant to be a transcript which can be run as a
prelude to other transcripts, creating helper functions, and including
a minimal subset of base in order to facilitate write nicer
transcripts which contain less boilerplate.

## Usage

``` ucm :hide
scratch/main> builtins.mergeio
scratch/main> load unison-src/transcripts-using-base/base.u
scratch/main> add
```

The test shows that `hex (fromHex str) == str` as expected.

``` unison :hide
test> hex.tests.ex1 = checks let
         s = "3984af9b"
         [hex (fromHex s) == s]
```

``` ucm :hide
scratch/main> test
```

Lets do some basic testing of our test harness to make sure its
working.

``` unison
testAutoClean : '{io2.IO}[Result]
testAutoClean _ =
  go: '{Stream Result, Exception, io2.IO, TempDirs} Text
  go _ =
    dir = newTempDir "autoclean"
    check "our temporary directory should exist" (isDirectory dir)
    dir

  handle (evalTest go) with cases
    { Exception.raise (Failure _ t _) -> _ } -> [Fail t]
    { (results, dir) } ->
       match io2.IO.isDirectory.impl dir with
         Right b -> if b
                    then results :+ (Fail "our temporary directory should no longer exist")
                    else results :+ (Ok "our temporary directory should no longer exist")
         Left (Failure _ t _) -> results :+ (Fail t)
```

``` ucm
scratch/main> add
scratch/main> io.test testAutoClean
```

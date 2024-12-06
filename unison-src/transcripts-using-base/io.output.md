# tests for built-in IO functions

Tests for IO builtins which wired to foreign haskell calls.

## Setup

You can skip the section which is just needed to make the transcript self-contained.

TempDirs/autoCleaned is an ability/hanlder which allows you to easily
create a scratch directory which will automatically get cleaned up.

``` unison
```

## Basic File Functions

### Creating/Deleting/Renaming Directories

Tests: createDirectory,
       isDirectory,
       fileExists,
       renameDirectory,
       deleteDirectory

``` unison
testCreateRename : '{io2.IO} [Result]
testCreateRename _ =
  test = 'let
    tempDir = newTempDir "fileio"
    fooDir = tempDir ++ "/foo"
    barDir = tempDir ++ "/bar"
    createDirectory.impl fooDir
    check "create a foo directory" (isDirectory fooDir)
    check "directory should exist" (fileExists fooDir)
    renameDirectory fooDir barDir
    check "foo should no longer exist" (not (fileExists fooDir))
    check "directory should no longer exist" (not (fileExists fooDir))
    check "bar should now exist" (fileExists barDir)

    bazDir = barDir ++ "/baz"
    createDirectory.impl bazDir
    removeDirectory.impl barDir

    check "removeDirectory works recursively" (not (isDirectory barDir))
    check "removeDirectory works recursively" (not (isDirectory bazDir))

  runTest test
```

``` ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:

      testCreateRename : '{IO} [Result]

```
``` ucm
.> add

  ⍟ I've added these definitions:

    testCreateRename : '{IO} [Result]

.> io.test testCreateRename

    New test results:

  ◉ testCreateRename   create a foo directory
  ◉ testCreateRename   directory should exist
  ◉ testCreateRename   foo should no longer exist
  ◉ testCreateRename   directory should no longer exist
  ◉ testCreateRename   bar should now exist
  ◉ testCreateRename   removeDirectory works recursively
  ◉ testCreateRename   removeDirectory works recursively

  ✅ 7 test(s) passing

  Tip: Use view testCreateRename to view the source of a test.

```
### Opening / Closing files

Tests: openFile
       closeFile
       isFileOpen

``` unison
testOpenClose : '{io2.IO} [Result]
testOpenClose _ =
  test = 'let
    tempDir = (newTempDir "seek")
    fooFile = tempDir ++ "/foo"
    handle1 = openFile fooFile FileMode.Write
    check "file should be open" (isFileOpen handle1)
    closeFile handle1
    check "file should be closed" (not (isFileOpen handle1))

  runTest test
```

``` ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:

      testOpenClose : '{IO} [Result]

```
``` ucm
.> add

  ⍟ I've added these definitions:

    testOpenClose : '{IO} [Result]

.> io.test testOpenClose

    New test results:

  ◉ testOpenClose   file should be open
  ◉ testOpenClose   file should be closed

  ✅ 2 test(s) passing

  Tip: Use view testOpenClose to view the source of a test.

```
### Seeking in open files

Tests: openFile
       putBytes
       closeFile
       isSeekable
       isFileEOF
       seekHandle
       getBytes

``` unison
testSeek : '{io2.IO} [Result]
testSeek _ =
  test = 'let
    tempDir = newTempDir "seek"
    emit (Ok "seeked")
    fooFile = tempDir ++ "/foo"
    handle1 = openFile fooFile FileMode.Append
    putBytes handle1 (toUtf8 "12345678")
    closeFile handle1

    handle3 = openFile fooFile FileMode.Read
    check "readable file should be seekable" (isSeekable handle3)
    check "shouldn't be the EOF" (not (isFileEOF handle3))
    expectU "we should be at position 0" 0 (handlePosition handle3)

    seekHandle handle3 AbsoluteSeek +1
    expectU "we should be at position 1" 1 (handlePosition handle3)
    bytes3a = getBytes handle3 1000
    text3a = Text.fromUtf8 bytes3a
    expectU "should be able to read our temporary file after seeking" "2345678" text3a
    closeFile handle3

  runTest test

testAppend : '{io2.IO} [Result]
testAppend _ =
  test = 'let
    tempDir = newTempDir "openFile"
    fooFile = tempDir ++ "/foo"
    handle1 = openFile fooFile FileMode.Write
    putBytes handle1 (toUtf8 "test1")
    closeFile handle1

    handle2 = openFile fooFile FileMode.Append
    putBytes handle2 (toUtf8 "test2")
    closeFile handle2

    handle3 = openFile fooFile FileMode.Read
    bytes3 = getBytes handle3 1000
    text3 = Text.fromUtf8 bytes3

    expectU "should be able to read our temporary file" "test1test2" text3

    closeFile handle3

  runTest test
```

``` ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:

      testAppend : '{IO} [Result]
      testSeek   : '{IO} [Result]

```
``` ucm
.> add

  ⍟ I've added these definitions:

    testAppend : '{IO} [Result]
    testSeek   : '{IO} [Result]

.> io.test testSeek

    New test results:

  ◉ testSeek   seeked
  ◉ testSeek   readable file should be seekable
  ◉ testSeek   shouldn't be the EOF
  ◉ testSeek   we should be at position 0
  ◉ testSeek   we should be at position 1
  ◉ testSeek   should be able to read our temporary file after seeking

  ✅ 6 test(s) passing

  Tip: Use view testSeek to view the source of a test.

.> io.test testAppend

    New test results:

  ◉ testAppend   should be able to read our temporary file

  ✅ 1 test(s) passing

  Tip: Use view testAppend to view the source of a test.

```
### SystemTime
``` unison
testSystemTime : '{io2.IO} [Result]
testSystemTime _ =
  test = 'let
    t = !systemTime
    check "systemTime should be sane" ((t > 1600000000) && (t < 2000000000))

  runTest test
```

``` ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:

      testSystemTime : '{IO} [Result]

```
``` ucm
.> add

  ⍟ I've added these definitions:

    testSystemTime : '{IO} [Result]

.> io.test testSystemTime

    New test results:

  ◉ testSystemTime   systemTime should be sane

  ✅ 1 test(s) passing

  Tip: Use view testSystemTime to view the source of a test.

```

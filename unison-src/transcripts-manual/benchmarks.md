``` ucm :hide
scratch/main> pull unison.public.base.releases.M4d base
scratch/main> pull runarorama.public.sort.data sort
```

``` unison :hide
benchmarkFilePath = FilePath "unison-src/transcripts-manual/benchmarks/output.bench.txt"
archiveFilePath = FilePath "unison-src/transcripts-manual/benchmarks/output"

timeit : Text -> '{IO,Exception} a ->{IO,Exception} a
timeit label a =
  before = !realtime
  r = !a
  after = !realtime
  elapsed = Duration.between before after
  elapsedText = Duration.toText elapsed
  go file =
    putText file ("\n" ++ label ++ " " ++ Int.toText (Duration.countMicroseconds elapsed) ++ "   # " ++ elapsedText)
    printLine ("\n\n ******** \n")
    printLine (label ++ " took " ++ elapsedText)
  bracket '(FilePath.open benchmarkFilePath FileMode.Append) Handle.close go
  r

prepare = do
  -- if benchmarkFilePath exists, move it to blah-<datetime>.txt for archive purposes
  use Text ++
  if FilePath.exists benchmarkFilePath then
    createDirectory archiveFilePath
    now = OffsetDateTime.toText (atUTC !realtime)
    timestamped = FilePath.toText archiveFilePath ++ "/" ++ now ++ "-bench.txt"
    renameFile benchmarkFilePath (FilePath timestamped)
  else
    ()
```

``` ucm :hide
scratch/main> add
scratch/main> run prepare
```

## Benchmarks

``` ucm
scratch/main> load "unison-src/transcripts-manual/benchmarks/each.u"
scratch/main> run main
```

``` ucm
scratch/main> load "unison-src/transcripts-manual/benchmarks/listmap.u"
scratch/main> run main
```

``` ucm
scratch/main> load "unison-src/transcripts-manual/benchmarks/listfilter.u"
scratch/main> run main
```

``` ucm
scratch/main> load "unison-src/transcripts-manual/benchmarks/random.u"
scratch/main> run main
```

``` ucm
scratch/main> load "unison-src/transcripts-manual/benchmarks/simpleloop.u"
scratch/main> run main
```

``` ucm
scratch/main> load "unison-src/transcripts-manual/benchmarks/fibonacci.u"
scratch/main> run main
```

``` ucm
scratch/main> load "unison-src/transcripts-manual/benchmarks/map.u"
scratch/main> run main
```

``` ucm
scratch/main> load "unison-src/transcripts-manual/benchmarks/natmap.u"
scratch/main> run main
```

``` ucm
scratch/main> load "unison-src/transcripts-manual/benchmarks/stm.u"
scratch/main> run main
```

``` ucm
scratch/main> load "unison-src/transcripts-manual/benchmarks/tmap.u"
scratch/main> run main
```

``` ucm
scratch/main> load "unison-src/transcripts-manual/benchmarks/array-sort.u"
scratch/main> run main
```

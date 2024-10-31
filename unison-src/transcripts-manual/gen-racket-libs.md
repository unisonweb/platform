When we start out, `./scheme-libs/racket` contains a bunch of library files that we'll need. They define the Unison builtins for Racket.

Next, we'll download the jit project and generate a few Racket files from it.

``` ucm
jit-setup/main> lib.install @unison/internal/releases/0.0.24
```

``` unison
go = generateSchemeBoot "scheme-libs/racket"
```

``` ucm
jit-setup/main> run go
```

After executing this, `scheme-libs/racket` will contain the full
complement of unison libraries for a given combination of ucm version
and @unison/internal version.

To set up racket to use these files, we need to create a package with
them. This is accomplished by running:

    raco pkg install -t dir scheme-libs/racket/unison

After, the runtime executable can be built with

    raco exe scheme-libs/racket/unison-runtime.rkt

and a distributable directory can be produced with:

    raco distribute <output-dir> scheme-libs/racket/unison-runtime

At that point, <output-dir> should contain the executable and all
dependencies necessary to run it.

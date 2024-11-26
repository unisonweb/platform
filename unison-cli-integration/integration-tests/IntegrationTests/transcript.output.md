# Integration test: transcript

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins

scratch/main> load ./unison-src/transcripts-using-base/base.u

scratch/main> add
```

``` unison
use lib.builtins

unique type MyBool = MyTrue | MyFalse

structural ability Break where
  break : ()

resume = cases
  { x } -> id x
  { break  -> k } ->
    void 5
    handle k () with resume

main : '{IO, Exception} ()
main = do
  match MyTrue with
    MyTrue -> match 0 with
      0 ->
        handle
          break
          printLine "Hello, world!"
        with resume
      _ -> ()
    _ -> ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      structural ability Break
      type MyBool
      main   : '{IO, Exception} ()
      resume : Request {g, Break} x -> x
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    structural ability Break
    type MyBool
    main   : '{IO, Exception} ()
    resume : Request {g, Break} x -> x

scratch/main> compile main ./unison-cli-integration/integration-tests/IntegrationTests/main
```

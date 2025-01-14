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

``` ucm
scratch/main> add
scratch/main> compile main ./unison-cli-integration/integration-tests/IntegrationTests/main
```

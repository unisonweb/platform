``` ucm
upgrade-handler/main> builtins.merge

  Done.
```

``` unison :hide
structural ability lib.Abort where
  abort : a

lib.foo_1_0_0.base_1_0_0.Abort.handler : Request {Abort} r -> r
lib.foo_1_0_0.base_1_0_0.Abort.handler = cases
    { r } -> r
    { abort -> _ } -> bug "error message 1"

bar = handle '(abort) with Abort.handler
```

``` ucm :hide
upgrade-handler/main> add
```

``` unison :hide
lib.foo_1_0_1.base_1_0_1.Abort.handler : Request {Abort} r -> r
lib.foo_1_0_1.base_1_0_1.Abort.handler = cases
    { r } -> r
    { abort -> _ } -> bug "error message 2"
```

``` ucm :hide
upgrade-handler/main> update
```

``` ucm :error
upgrade-handler/main> upgrade foo_1_0_0 foo_1_0_1

  I couldn't automatically upgrade foo_1_0_0 to foo_1_0_1.
  However, I've added the definitions that need attention to the
  top of scratch.u.

  When you're done, you can run

    upgrade.commit

  to merge your changes back into main and delete the temporary
  branch. Or, if you decide to cancel the upgrade instead, you
  can run

    delete.branch /upgrade-foo_1_0_0-to-foo_1_0_1

  to delete the temporary branch and switch back to main.
```

``` unison :added-by-ucm scratch.u
bar : '{Abort} r
bar = handle do abort with lib.foo_1_0_0.base_1_0_0.Abort.handler
```

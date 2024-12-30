# The `merge` command

The `merge` command merges together two branches in the same project: the current branch (unspecificed), and the target
branch. For example, to merge `topic` into `main`, switch to `main` and run `merge topic`:

``` ucm
scratch/main> help merge

  merge
  `merge /branch` merges `branch` into the current branch

scratch/main> help merge.commit

  merge.commit (or commit.merge)
  `merge.commit` merges a temporary branch created by the
  `merge` command back into its parent branch, and removes the
  temporary branch.

  For example, if you've done `merge topic` from main, then
  `merge.commit` is equivalent to doing

    * switch /main
    * merge /merge-topic-into-main
    * delete.branch /merge-topic-into-main
```

Let's see a simple unconflicted merge in action: Alice (us) and Bob (them) add different terms. The merged result
contains both additions.

## Basic merge: two unconflicted adds

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` ucm :hide
scratch/main> branch alice
```

Alice's adds:

``` unison :hide
foo : Text
foo = "alices foo"
```

``` ucm :hide
scratch/alice> add

scratch/main> branch bob
```

Bob's adds:

``` unison :hide
bar : Text
bar = "bobs bar"
```

``` ucm :hide
scratch/bob> add
```

Merge result:

``` ucm
scratch/alice> merge /bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Loading dependents of changes...

  Loading and merging library dependencies...

  Rendering Unison file...

  Typechecking Unison file...

  I merged scratch/bob into scratch/alice.

scratch/alice> view foo bar

  bar : Text
  bar = "bobs bar"

  foo : Text
  foo = "alices foo"
```

``` ucm :hide
scratch/main> project.delete scratch
```

## Basic merge: two identical adds

If Alice and Bob also happen to add the same definition, that's not a conflict.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins

scratch/main> branch alice
```

Alice's adds:

``` unison :hide
foo : Text
foo = "alice and bobs foo"
```

``` ucm :hide
scratch/alice> add

scratch/main> branch bob
```

Bob's adds:

``` unison :hide
foo : Text
foo = "alice and bobs foo"

bar : Text
bar = "bobs bar"
```

``` ucm :hide
scratch/bob> add
```

Merge result:

``` ucm
scratch/alice> merge /bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Loading dependents of changes...

  Loading and merging library dependencies...

  Rendering Unison file...

  Typechecking Unison file...

  I merged scratch/bob into scratch/alice.

scratch/alice> view foo bar

  bar : Text
  bar = "bobs bar"

  foo : Text
  foo = "alice and bobs foo"
```

``` ucm :hide
scratch/main> project.delete scratch
```

## Simple update propagation

Updates that occur in one branch are propagated to the other. In this example, Alice updates `foo`, while Bob adds a new dependent `bar` of the original `foo`. When Bob's branch is merged into Alice's, her update to `foo` is propagated to his `bar`.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

Original branch:

``` unison :hide
foo : Text
foo = "old foo"
```

``` ucm :hide
scratch/main> add

scratch/main> branch alice
```

Alice's updates:

``` unison :hide
foo : Text
foo = "new foo"
```

``` ucm :hide
scratch/alice> update

scratch/main> branch bob
```

Bob's adds:

``` unison :hide
bar : Text
bar = foo ++ " - " ++ foo
```

``` ucm
scratch/bob> display bar

  "old foo - old foo"
```

``` ucm :hide
scratch/bob> add
```

Merge result:

``` ucm
scratch/alice> merge /bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Loading dependents of changes...

  Loading and merging library dependencies...

  Rendering Unison file...

  Typechecking Unison file...

  I merged scratch/bob into scratch/alice.

scratch/alice> view foo bar

  bar : Text
  bar =
    use Text ++
    foo ++ " - " ++ foo

  foo : Text
  foo = "new foo"

scratch/alice> display bar

  "old foo - old foo"
```

``` ucm :hide
scratch/main> project.delete scratch
```

## Update propagation with common dependent

We classify something as an update if its "syntactic hash"—not its normal Unison hash—differs from the original definition. This allows us to cleanly merge unconflicted updates that were individually propagated to a common dependent.

Let's see an example. We have `foo`, which depends on `bar` and `baz`. Alice updates `bar` (propagating to `foo`), and Bob updates `baz` (propagating to `foo`). When we merge their updates, both updates will be reflected in the final `foo`.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

Original branch:

``` unison :hide
foo : Text
foo = "foo" ++ " - " ++ bar ++ " - " ++ baz

bar : Text
bar = "old bar"

baz : Text
baz = "old baz"
```

``` ucm :hide
scratch/main> add

scratch/main> branch alice
```

Alice's updates:

``` unison :hide
bar : Text
bar = "alices bar"
```

``` ucm :hide
scratch/alice> update
```

``` ucm
scratch/alice> display foo

  "foo - alices bar - old baz"
```

``` ucm :hide
scratch/main> branch bob
```

Bob's updates:

``` unison :hide
baz : Text
baz = "bobs baz"
```

``` ucm :hide
scratch/bob> update
```

``` ucm
scratch/bob> display foo

  "foo - old bar - bobs baz"
```

Merge result:

``` ucm
scratch/alice> merge /bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Loading dependents of changes...

  Loading and merging library dependencies...

  Rendering Unison file...

  Typechecking Unison file...

  I merged scratch/bob into scratch/alice.

scratch/alice> view foo bar baz

  bar : Text
  bar = "alices bar"

  baz : Text
  baz = "bobs baz"

  foo : Text
  foo =
    use Text ++
    "foo" ++ " - " ++ bar ++ " - " ++ baz

scratch/alice> display foo

  "foo - alices bar - bobs baz"
```

``` ucm :hide
scratch/main> project.delete scratch
```

## Propagating an update to an update

Of course, it's also possible for Alice's update to propagate to one of Bob's updates. In this example, `foo` depends on `bar` which depends on `baz`. Alice updates `baz`, propagating to `bar` and `foo`, while Bob updates `bar` (to something that still depends on `foo`), propagating to `baz`. The merged result will have Alice's update to `foo` incorporated into Bob's updated `bar`, and both updates will propagate to `baz`.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

Original branch:

``` unison :hide
foo : Text
foo = "old foo" ++ " - " ++ bar

bar : Text
bar = "old bar" ++ " - " ++ baz

baz : Text
baz = "old baz"
```

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> display foo

  "old foo - old bar - old baz"
```

``` ucm :hide
scratch/main> branch alice
```

Alice's updates:

``` unison :hide
baz : Text
baz = "alices baz"
```

``` ucm :hide
scratch/alice> update
```

``` ucm
scratch/alice> display foo

  "old foo - old bar - alices baz"
```

``` ucm :hide
scratch/main> branch bob
```

Bob's updates:

``` unison :hide
bar : Text
bar = "bobs bar" ++ " - " ++ baz
```

``` ucm :hide
scratch/bob> update
```

``` ucm
scratch/bob> display foo

  "old foo - bobs bar - old baz"
```

Merge result:

``` ucm
scratch/alice> merge /bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Loading dependents of changes...

  Loading and merging library dependencies...

  Rendering Unison file...

  Typechecking Unison file...

  I merged scratch/bob into scratch/alice.

scratch/alice> view foo bar baz

  bar : Text
  bar =
    use Text ++
    "bobs bar" ++ " - " ++ baz

  baz : Text
  baz = "alices baz"

  foo : Text
  foo =
    use Text ++
    "old foo" ++ " - " ++ bar

scratch/alice> display foo

  "old foo - bobs bar - alices baz"
```

``` ucm :hide
scratch/main> project.delete scratch
```

## Update + delete isn't (currently) a conflict

We don't currently consider "update + delete" a conflict like Git does. In this situation, the delete is just ignored, allowing the update to proceed.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

Original branch:

``` unison :hide
foo : Text
foo = "old foo"
```

``` ucm :hide
scratch/main> add

scratch/main> branch alice
```

Alice's updates:

``` unison :hide
foo : Text
foo = "alices foo"
```

``` ucm :hide
scratch/alice> update

scratch/main> branch bob
```

Bob's changes:

``` ucm
scratch/bob> delete.term foo

  Done.
```

Merge result:

``` ucm
scratch/alice> merge /bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Loading dependents of changes...

  Loading and merging library dependencies...

  Rendering Unison file...

  Typechecking Unison file...

  I merged scratch/bob into scratch/alice.

scratch/alice> view foo

  foo : Text
  foo = "alices foo"
```

``` ucm :hide
scratch/main> project.delete scratch
```

In a future version, we'd like to give the user a warning at least.

## Library dependencies don't create merge conflicts

Library dependencies don't cause merge conflicts, the library dependencies are just unioned together. If two library dependencies have the same name but different namespace hashes, then the merge algorithm makes up two fresh names.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

Alice's adds:

``` ucm :hide
scratch/main> branch alice
```

``` unison :hide
lib.alice.foo : Nat
lib.alice.foo = 17

lib.bothSame.bar : Nat
lib.bothSame.bar = 18

lib.bothDifferent.baz : Nat
lib.bothDifferent.baz = 19
```

``` ucm :hide
scratch/alice> add

scratch/main> branch bob
```

Bob's adds:

``` unison :hide
lib.bob.foo : Nat
lib.bob.foo = 20

lib.bothSame.bar : Nat
lib.bothSame.bar = 18

lib.bothDifferent.baz : Nat
lib.bothDifferent.baz = 21
```

``` ucm :hide
scratch/bob> add
```

Merge result:

``` ucm
scratch/alice> merge bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Loading dependents of changes...

  Loading and merging library dependencies...

  Rendering Unison file...

  Typechecking Unison file...

  I merged scratch/bob into scratch/alice.

scratch/alice> view foo bar baz

  lib.alice.foo : Nat
  lib.alice.foo = 17

  lib.bob.foo : Nat
  lib.bob.foo = 20

  lib.bothDifferent__0.baz : Nat
  lib.bothDifferent__0.baz = 19

  lib.bothDifferent__1.baz : Nat
  lib.bothDifferent__1.baz = 21

  lib.bothSame.bar : Nat
  lib.bothSame.bar = 18
```

``` ucm :hide
scratch/main> project.delete scratch
```

## No-op merge (Bob = Alice)

If Bob is equals Alice, then merging Bob into Alice looks like this.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` ucm
scratch/main> branch alice

  Done. I've created the alice branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /alice`.

scratch/main> branch bob

  Done. I've created the bob branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /bob`.

scratch/alice> merge /bob

  😶

  scratch/alice was already up-to-date with scratch/bob.
```

``` ucm :hide
scratch/main> project.delete scratch
```

## No-op merge (Bob \< Alice)

If Bob is behind Alice, then merging Bob into Alice looks like this.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` ucm
scratch/main> branch alice

  Done. I've created the alice branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /alice`.

scratch/main> branch bob

  Done. I've created the bob branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /bob`.
```

Alice's addition:

``` unison :hide
foo : Text
foo = "foo"
```

``` ucm
scratch/alice> add

  ⍟ I've added these definitions:

    foo : Text

scratch/alice> merge /bob

  😶

  scratch/alice was already up-to-date with scratch/bob.
```

``` ucm :hide
scratch/main> project.delete scratch
```

## Fast-forward merge (Bob \> Alice)

If Bob is ahead of Alice, then merging Bob into Alice looks like this.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` ucm
scratch/main> branch alice

  Done. I've created the alice branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /alice`.

scratch/main> branch bob

  Done. I've created the bob branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /bob`.
```

Bob's addition:

``` unison :hide
foo : Text
foo = "foo"
```

``` ucm
scratch/bob> add

  ⍟ I've added these definitions:

    foo : Text

scratch/alice> merge /bob

  I fast-forward merged scratch/bob into scratch/alice.
```

``` ucm :hide
scratch/main> project.delete scratch
```

## No-op merge: merge empty namespace into empty namespace

``` ucm
scratch/main> branch topic

  Done. I've created the topic branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic`.

scratch/main> merge /topic

  😶

  scratch/main was already up-to-date with scratch/topic.
```

``` ucm :hide
scratch/main> project.delete scratch
```

## Merge failure: someone deleted something

If either Alice or Bob delete something, so long as the other person didn't update it (in which case we ignore the delete, as explained above), then the delete goes through.

This can cause merge failures due to out-of-scope identifiers, and the user may have to do some digging around to find what the deleted name used to refer to. In a future version, we would emit a \[better\] warning at least.

In this example, Alice deletes `foo`, while Bob adds a new dependent of `foo`.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

Original branch:

``` unison :hide
foo : Text
foo = "foo"
```

``` ucm :hide
scratch/main> add

scratch/main> branch alice
```

Alice's delete:

``` ucm
scratch/alice> delete.term foo

  Done.
```

``` ucm :hide
scratch/main> branch bob
```

Bob's new code that depends on `foo`:

``` unison :hide
bar : Text
bar = foo ++ " - " ++ foo
```

``` ucm :error
scratch/bob> add

  ⍟ I've added these definitions:

    bar : Text

scratch/alice> merge /bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Loading dependents of changes...

  Loading and merging library dependencies...

  Rendering Unison file...

  Typechecking Unison file...

  I couldn't automatically merge scratch/bob into scratch/alice.
  However, I've added the definitions that need attention to the
  top of scratch.u.

  When you're done, you can run

    merge.commit

  to merge your changes back into alice and delete the temporary
  branch. Or, if you decide to cancel the merge instead, you can
  run

    delete.branch /merge-bob-into-alice

  to delete the temporary branch and switch back to alice.
```

``` unison :added-by-ucm scratch.u
bar : Text
bar =
  use Text ++
  foo ++ " - " ++ foo

```

``` ucm :hide
scratch/main> project.delete scratch
```

## Merge failure: type error

It may be Alice's and Bob's changes merge together cleanly in the sense that there's no textual conflicts, yet the resulting namespace doesn't typecheck.

In this example, Alice updates a `Text` to a `Nat`, while Bob adds a new dependent of the `Text`. Upon merging, propagating Alice's update to Bob's dependent causes a typechecking failure.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

Original branch:

``` unison :hide
foo : Text
foo = "foo"
```

``` ucm :hide
scratch/main> add

scratch/main> branch alice
```

Alice's update:

``` unison :hide
foo : Nat
foo = 100
```

``` ucm :hide
scratch/alice> update

scratch/main> branch bob
```

Bob's new definition:

``` unison :hide
bar : Text
bar = foo ++ " - " ++ foo
```

``` ucm :hide
scratch/bob> update
```

``` ucm :error
scratch/alice> merge /bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Loading dependents of changes...

  Loading and merging library dependencies...

  Rendering Unison file...

  Typechecking Unison file...

  I couldn't automatically merge scratch/bob into scratch/alice.
  However, I've added the definitions that need attention to the
  top of scratch.u.

  When you're done, you can run

    merge.commit

  to merge your changes back into alice and delete the temporary
  branch. Or, if you decide to cancel the merge instead, you can
  run

    delete.branch /merge-bob-into-alice

  to delete the temporary branch and switch back to alice.
```

``` unison :added-by-ucm scratch.u
bar : Text
bar =
  use Text ++
  foo ++ " - " ++ foo

```

``` ucm :hide
scratch/main> project.delete scratch
```

## Merge failure: simple term conflict

Alice and Bob may disagree about the definition of a term. In this case, the conflicted term and all of its dependents
are presented to the user to resolve.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

Original branch:

``` unison :hide
foo : Text
foo = "old foo"

bar : Text
bar = "old bar"
```

``` ucm :hide
scratch/main> add

scratch/main> branch alice
```

Alice's changes:

``` unison :hide
foo : Text
foo = "alices foo"

bar : Text
bar = "alices bar"

qux : Text
qux = "alices qux depends on alices foo" ++ foo
```

``` ucm :hide
scratch/alice> update

scratch/main> branch bob
```

Bob's changes:

``` unison :hide
foo : Text
foo = "bobs foo"

baz : Text
baz = "bobs baz"
```

``` ucm :hide
scratch/bob> update
```

``` ucm :error
scratch/alice> merge /bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Loading dependents of changes...

  Loading and merging library dependencies...

  Rendering Unison file...

  I couldn't automatically merge scratch/bob into scratch/alice.
  However, I've added the definitions that need attention to the
  top of scratch.u.

  When you're done, you can run

    merge.commit

  to merge your changes back into alice and delete the temporary
  branch. Or, if you decide to cancel the merge instead, you can
  run

    delete.branch /merge-bob-into-alice

  to delete the temporary branch and switch back to alice.
```

``` unison :added-by-ucm scratch.u
-- scratch/alice
foo : Text
foo = "alices foo"

-- scratch/bob
foo : Text
foo = "bobs foo"

-- The definitions below are not conflicted, but they each depend on one or more
-- conflicted definitions above.

qux : Text
qux =
  use Text ++
  "alices qux depends on alices foo" ++ foo

```

``` ucm
scratch/merge-bob-into-alice> view bar baz

  bar : Text
  bar = "alices bar"

  baz : Text
  baz = "bobs baz"
```

``` ucm :hide
scratch/main> project.delete scratch
```

## Merge failure: simple type conflict

Ditto for types; if the hashes don't match, it's a conflict. In this example, Alice and Bob do different things to the same constructor. However, any explicit changes to the same type will result in a conflict, including changes that could concievably be merged (e.g. Alice and Bob both add a new constructor, or edit different constructors).

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

Original branch:

``` unison :hide
unique type Foo = MkFoo Nat
```

``` ucm :hide
scratch/main> add

scratch/main> branch alice
```

Alice's changes:

``` unison :hide
unique type Foo = MkFoo Nat Nat
```

``` ucm :hide
scratch/alice> update

scratch/main> branch bob
```

Bob's changes:

``` unison :hide
unique type Foo = MkFoo Nat Text
```

``` ucm :hide
scratch/bob> update
```

``` ucm :error
scratch/alice> merge /bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Loading dependents of changes...

  Loading and merging library dependencies...

  Rendering Unison file...

  I couldn't automatically merge scratch/bob into scratch/alice.
  However, I've added the definitions that need attention to the
  top of scratch.u.

  When you're done, you can run

    merge.commit

  to merge your changes back into alice and delete the temporary
  branch. Or, if you decide to cancel the merge instead, you can
  run

    delete.branch /merge-bob-into-alice

  to delete the temporary branch and switch back to alice.
```

``` unison :added-by-ucm scratch.u
-- scratch/alice
type Foo = MkFoo Nat Nat

-- scratch/bob
type Foo = MkFoo Nat Text

```

``` ucm :hide
scratch/main> project.delete scratch
```

## Merge failure: type-update + constructor-rename conflict

We model the renaming of a type's constructor as an update, so if Alice updates a type and Bob renames one of its constructors (even without changing its structure), we consider it a conflict.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

Original branch:

``` unison :hide
unique type Foo = Baz Nat | Qux Text
```

``` ucm :hide
scratch/main> add

scratch/main> branch alice
```

Alice's changes `Baz Nat` to `Baz Nat Nat`

``` unison :hide
unique type Foo = Baz Nat Nat | Qux Text
```

``` ucm :hide
scratch/alice> update

scratch/main> branch bob
```

Bob's renames `Qux` to `BobQux`:

``` ucm
scratch/bob> move.term Foo.Qux Foo.BobQux

  Done.
```

``` ucm :error
scratch/alice> merge /bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Loading dependents of changes...

  Loading and merging library dependencies...

  Rendering Unison file...

  I couldn't automatically merge scratch/bob into scratch/alice.
  However, I've added the definitions that need attention to the
  top of scratch.u.

  When you're done, you can run

    merge.commit

  to merge your changes back into alice and delete the temporary
  branch. Or, if you decide to cancel the merge instead, you can
  run

    delete.branch /merge-bob-into-alice

  to delete the temporary branch and switch back to alice.
```

``` unison :added-by-ucm scratch.u
-- scratch/alice
type Foo = Baz Nat Nat | Qux Text

-- scratch/bob
type Foo = BobQux Text | Baz Nat

```

``` ucm :hide
scratch/main> project.delete scratch
```

## Merge failure: constructor-rename conflict

Here is another example demonstrating that constructor renames are modeled as updates.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

Original branch:

``` unison :hide
unique type Foo = Baz Nat | Qux Text
```

``` ucm :hide
scratch/main> add

scratch/main> branch alice
```

Alice's rename:

``` ucm
scratch/alice> move.term Foo.Baz Foo.Alice

  Done.
```

``` ucm :hide
scratch/main> branch bob
```

Bob's rename:

``` ucm
scratch/bob> move.term Foo.Qux Foo.Bob

  Done.
```

``` ucm :error
scratch/alice> merge bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Loading dependents of changes...

  Loading and merging library dependencies...

  Rendering Unison file...

  I couldn't automatically merge scratch/bob into scratch/alice.
  However, I've added the definitions that need attention to the
  top of scratch.u.

  When you're done, you can run

    merge.commit

  to merge your changes back into alice and delete the temporary
  branch. Or, if you decide to cancel the merge instead, you can
  run

    delete.branch /merge-bob-into-alice

  to delete the temporary branch and switch back to alice.
```

``` unison :added-by-ucm scratch.u
-- scratch/alice
type Foo = Qux Text | Alice Nat

-- scratch/bob
type Foo = Bob Text | Baz Nat

```

``` ucm :hide
scratch/main> project.delete scratch
```

## Merge failure: non-constructor/constructor conflict

A constructor on one side can conflict with a regular term definition on the other.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` ucm :hide
scratch/main> branch alice
```

Alice's additions:

``` unison :hide
my.cool.thing : Nat
my.cool.thing = 17
```

``` ucm :hide
scratch/alice> add

scratch/main> branch bob
```

Bob's additions:

``` unison :hide
unique ability my.cool where
  thing : Nat -> Nat
```

``` ucm :hide
scratch/bob> add
```

``` ucm :error
scratch/alice> merge bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Loading dependents of changes...

  Loading and merging library dependencies...

  Rendering Unison file...

  I couldn't automatically merge scratch/bob into scratch/alice.
  However, I've added the definitions that need attention to the
  top of scratch.u.

  When you're done, you can run

    merge.commit

  to merge your changes back into alice and delete the temporary
  branch. Or, if you decide to cancel the merge instead, you can
  run

    delete.branch /merge-bob-into-alice

  to delete the temporary branch and switch back to alice.
```

``` unison :added-by-ucm scratch.u
-- scratch/alice
my.cool.thing : Nat
my.cool.thing = 17

-- scratch/bob
ability my.cool where thing : Nat ->{cool} Nat

```

``` ucm :hide
scratch/main> project.delete scratch
```

## Merge failure: type/type conflict with term/constructor conflict

Here's a subtle situation where a new type is added on each side of the merge, and an existing term is replaced with a constructor of one of the types.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

Original branch:

``` unison :hide
Foo.Bar : Nat
Foo.Bar = 17
```

``` ucm :hide
scratch/main> add

scratch/main> branch alice
```

Alice adds this type `Foo` with constructor `Foo.Alice`:

``` unison :hide
unique type Foo = Alice Nat
```

``` ucm :hide
scratch/alice> add

scratch/main> branch bob
```

Bob adds the type `Foo` with constructor `Foo.Bar`, replacing the original `Foo.Bar` term:

``` ucm
scratch/bob> delete.term Foo.Bar

  Done.
```

``` unison :hide
unique type Foo = Bar Nat Nat
```

``` ucm :hide
scratch/bob> add
```

These won't cleanly merge.

``` ucm :error
scratch/alice> merge bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Loading dependents of changes...

  Loading and merging library dependencies...

  Rendering Unison file...

  I couldn't automatically merge scratch/bob into scratch/alice.
  However, I've added the definitions that need attention to the
  top of scratch.u.

  When you're done, you can run

    merge.commit

  to merge your changes back into alice and delete the temporary
  branch. Or, if you decide to cancel the merge instead, you can
  run

    delete.branch /merge-bob-into-alice

  to delete the temporary branch and switch back to alice.
```

``` unison :added-by-ucm scratch.u
-- scratch/alice
Foo.Bar : Nat
Foo.Bar = 17

-- scratch/alice
type Foo = Alice Nat

-- scratch/bob
type Foo = Bar Nat Nat

```

``` ucm :hide
scratch/main> project.delete scratch
```

Here's a more involved example that demonstrates the same idea.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

In the LCA, we have a type with two constructors, and some term.

``` unison :hide
unique type Foo
  = Bar.Baz Nat
  | Bar.Qux Nat Nat

Foo.Bar.Hello : Nat
Foo.Bar.Hello = 17
```

``` ucm :hide
scratch/main> add

scratch/main> branch alice
```

Alice deletes this type entirely, and repurposes its constructor names for other terms. She also updates the term.

``` ucm :hide
scratch/alice> delete.type Foo

scratch/alice> delete.term Foo.Bar.Baz

scratch/alice> delete.term Foo.Bar.Qux
```

``` ucm :hide
scratch/alice> update
```

``` ucm
scratch/alice> view Foo.Bar.Baz Foo.Bar.Qux Foo.Bar.Hello

  Foo.Bar.Baz : Nat
  Foo.Bar.Baz = 100

  Foo.Bar.Hello : Nat
  Foo.Bar.Hello = 18

  Foo.Bar.Qux : Nat
  Foo.Bar.Qux = 200
```

Bob, meanwhile, first deletes the term, then sort of deletes the type and re-adds it under another name, but one constructor's fully qualified names doesn't actually change. The other constructor reuses the name of the deleted term.

``` ucm :hide
scratch/main> branch bob

scratch/bob> delete.term Foo.Bar.Hello

scratch/bob> move.type Foo Foo.Bar

scratch/bob> move.term Foo.Bar.Qux Foo.Bar.Hello
```

``` ucm
scratch/bob> view Foo.Bar

  type Foo.Bar = Hello Nat Nat | Baz Nat
```

At this point, Bob and alice have both updated the name `Foo.Bar.Hello` in different ways, so that's a conflict. Therefore, Bob's entire type (`Foo.Bar` with constructors `Foo.Bar.Baz` and `Foo.Bar.Hello`) gets rendered into the scratch file.

Notably, Alice's "unconflicted" update on the name "Foo.Bar.Baz" (because she changed its hash and Bob didn't touch it) is nonetheless considered conflicted with Bob's "Foo.Bar.Baz".

``` ucm :error
scratch/alice> merge bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Loading dependents of changes...

  Loading and merging library dependencies...

  Rendering Unison file...

  I couldn't automatically merge scratch/bob into scratch/alice.
  However, I've added the definitions that need attention to the
  top of scratch.u.

  When you're done, you can run

    merge.commit

  to merge your changes back into alice and delete the temporary
  branch. Or, if you decide to cancel the merge instead, you can
  run

    delete.branch /merge-bob-into-alice

  to delete the temporary branch and switch back to alice.
```

``` unison :added-by-ucm scratch.u
-- scratch/alice
Foo.Bar.Baz : Nat
Foo.Bar.Baz = 100

-- scratch/alice
Foo.Bar.Hello : Nat
Foo.Bar.Hello = 18

-- scratch/bob
type Foo.Bar = Hello Nat Nat | Baz Nat

```

``` ucm :hide
scratch/main> project.delete scratch
```

## Merge algorithm quirk: add/add unique types

Currently, two unique types created by Alice and Bob will be considered in conflict, even if they "look the same".
The result may be confusing to a user – a file containing two identical-looking copies of a unique type is rendered,
which is a parse error.

We will resolve this situation automatically in a future version.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` ucm :hide
scratch/main> branch alice
```

Alice's additions:

``` unison :hide
unique type Foo = Bar

alice : Foo -> Nat
alice _ = 18
```

``` ucm :hide
scratch/alice> add

scratch/main> branch bob
```

Bob's additions:

``` unison :hide
unique type Foo = Bar

bob : Foo -> Nat
bob _ = 19
```

``` ucm :hide
scratch/bob> add
```

``` ucm :error
scratch/alice> merge bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Loading dependents of changes...

  Loading and merging library dependencies...

  Rendering Unison file...

  I couldn't automatically merge scratch/bob into scratch/alice.
  However, I've added the definitions that need attention to the
  top of scratch.u.

  When you're done, you can run

    merge.commit

  to merge your changes back into alice and delete the temporary
  branch. Or, if you decide to cancel the merge instead, you can
  run

    delete.branch /merge-bob-into-alice

  to delete the temporary branch and switch back to alice.
```

``` unison :added-by-ucm scratch.u
-- scratch/alice
type Foo
  = Bar

-- scratch/bob
type Foo
  = Bar

-- The definitions below are not conflicted, but they each depend on one or more
-- conflicted definitions above.

alice : Foo -> Nat
alice _ = 18

bob : Foo -> Nat
bob _ = 19

```

``` ucm :hide
scratch/main> project.delete scratch
```

## `merge.commit` example (success)

After merge conflicts are resolved, you can use `merge.commit` rather than `switch` + `merge` + `branch.delete` to
"commit" your changes.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

Original branch:

``` unison :hide
foo : Text
foo = "old foo"
```

``` ucm :hide
scratch/main> add

scratch/main> branch alice
```

Alice's changes:

``` unison :hide
foo : Text
foo = "alices foo"
```

``` ucm :hide
scratch/alice> update

scratch/main> branch bob
```

Bob's changes:

``` unison :hide
foo : Text
foo = "bobs foo"
```

Attempt to merge:

``` ucm :hide
scratch/bob> update
```

``` ucm :error
scratch/alice> merge /bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Loading dependents of changes...

  Loading and merging library dependencies...

  Rendering Unison file...

  I couldn't automatically merge scratch/bob into scratch/alice.
  However, I've added the definitions that need attention to the
  top of scratch.u.

  When you're done, you can run

    merge.commit

  to merge your changes back into alice and delete the temporary
  branch. Or, if you decide to cancel the merge instead, you can
  run

    delete.branch /merge-bob-into-alice

  to delete the temporary branch and switch back to alice.
```

``` unison :added-by-ucm scratch.u
-- scratch/alice
foo : Text
foo = "alices foo"

-- scratch/bob
foo : Text
foo = "bobs foo"

```

Resolve conflicts and commit:

``` unison
foo : Text
foo = "alice and bobs foo"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : Text
```

``` ucm
scratch/merge-bob-into-alice> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/merge-bob-into-alice> merge.commit

  I fast-forward merged scratch/merge-bob-into-alice into
  scratch/alice.

scratch/alice> view foo

  foo : Text
  foo = "alice and bobs foo"

scratch/alice> branches

       Branch   Remote branch
  1.   alice    
  2.   bob      
  3.   main     
```

``` ucm :hide
scratch/main> project.delete scratch
```

## `merge.commit` example (failure)

`merge.commit` can only be run on a "merge branch".

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` ucm
scratch/main> branch topic

  Done. I've created the topic branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic`.
```

``` ucm :error
scratch/topic> merge.commit

  It doesn't look like there's a merge in progress.
```

``` ucm :hide
scratch/main> project.delete scratch
```

## Precondition violations

There are a number of conditions under which we can't perform a merge, and the user will have to fix up the namespace(s) manually before attempting to merge again.

### Conflicted aliases

If `foo` and `bar` are aliases in the nearest common ancestor, but not in Alice's branch, then we don't know whether to update Bob's dependents to Alice's `foo` or Alice's `bar` (and vice-versa).

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

Original branch:

``` unison :hide
foo : Nat
foo = 100

bar : Nat
bar = 100
```

``` ucm :hide
scratch/main> add

scratch/main> branch alice
```

Alice's updates:

``` unison :hide
foo : Nat
foo = 200

bar : Nat
bar = 300
```

``` ucm :hide
scratch/alice> update

scratch/main> branch bob
```

Bob's addition:

``` unison :hide
baz : Text
baz = "baz"
```

``` ucm :hide
scratch/bob> add
```

``` ucm :error
scratch/alice> merge /bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Sorry, I wasn't able to perform the merge:

  On the merge ancestor, bar and foo were aliases for the same
  term, but on scratch/alice the names have different
  definitions currently. I'd need just a single new definition
  to use in their dependents when I merge.

  Please fix up scratch/alice to resolve this. For example,

    * `update` the definitions to be the same again, so that
      there's nothing for me to decide.
    * `move` or `delete` all but one of the definitions; I'll
      use the remaining name when propagating updates. (You can
      `move` it back after the merge.)

  and then try merging again.
```

``` ucm :hide
scratch/main> project.delete scratch
```

### Conflict involving builtin

We don't have a way of rendering a builtin in a scratch file, where users resolve merge conflicts. Thus, if there is a
conflict involving a builtin, we can't perform a merge.

One way to fix this in the future would be to introduce a syntax for defining aliases in the scratch file.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` ucm :hide
scratch/main> branch alice
```

Alice's branch:

``` ucm
scratch/alice> alias.type lib.builtins.Nat MyNat

  Done.
```

Bob's branch:

``` ucm :hide
scratch/main> branch bob
```

``` unison :hide
unique type MyNat = MyNat Nat
```

``` ucm :hide
scratch/bob> add
```

``` ucm :error
scratch/alice> merge /bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Sorry, I wasn't able to perform the merge:

  There's a merge conflict on type MyNat, but it's a builtin on
  one or both branches. I can't yet handle merge conflicts
  involving builtins.

  Please eliminate this conflict by updating one branch or the
  other, making MyNat the same on both branches, or making
  neither of them a builtin, and then try the merge again.
```

``` ucm :hide
scratch/main> project.delete scratch
```

### Constructor alias

Each naming of a decl may not have more than one name for each constructor, within the decl's namespace.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` ucm :hide
scratch/main> branch alice
```

Alice's branch:

``` unison :hide
unique type Foo = Bar
```

``` ucm :hide
scratch/alice> add
```

``` ucm
scratch/alice> alias.term Foo.Bar Foo.some.other.Alias

  Done.
```

Bob's branch:

``` ucm :hide
scratch/main> branch bob
```

``` unison :hide
bob : Nat
bob = 100
```

``` ucm :hide
scratch/bob> add
```

``` ucm :error
scratch/alice> merge /bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Sorry, I wasn't able to perform the merge:

  On scratch/alice, the type Foo has a constructor with multiple
  names, and I can't perform a merge in this situation:

    * Foo.Bar
    * Foo.some.other.Alias

  Please delete all but one name for each constructor, and then
  try merging again.
```

``` ucm :hide
scratch/main> project.delete scratch
```

### Missing constructor name

Each naming of a decl must have a name for each constructor, within the decl's namespace.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

Alice's branch:

``` ucm :hide
scratch/main> branch alice
```

``` unison :hide
unique type Foo = Bar
```

``` ucm :hide
scratch/alice> add
```

``` ucm
scratch/alice> delete.term Foo.Bar

  Done.
```

Bob's branch:

``` ucm :hide
scratch/main> branch /bob
```

``` unison :hide
bob : Nat
bob = 100
```

``` ucm :hide
scratch/bob> add
```

``` ucm :error
scratch/alice> merge /bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Sorry, I wasn't able to perform the merge:

  On scratch/alice, the type Foo has some constructors with
  missing names, and I can't perform a merge in this situation.

  You can use `view Foo` and
  `alias.term <hash> Foo.<ConstructorName>` to give names to
  each unnamed constructor, and then try the merge again.
```

``` ucm :hide
scratch/main> project.delete scratch
```

### Nested decl alias

A decl cannot be aliased within the namespace of another of its aliased.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

Alice's branch:

``` ucm :hide
scratch/main> branch alice
```

``` unison :hide
structural type A = B Nat | C Nat Nat
structural type A.inner.X = Y Nat | Z Nat Nat
```

``` ucm :hide
scratch/alice> add
```

``` ucm
scratch/alice> names A

  Type
  Hash:  #65mdg7015r
  Names: A A.inner.X
```

Bob's branch:

``` ucm :hide
scratch/main> branch bob
```

``` unison :hide
bob : Nat
bob = 100
```

``` ucm :hide
scratch/bob> add
```

``` ucm :error
scratch/alice> merge /bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  On scratch/alice, the type A.inner.X is an alias of A. I'm not
  able to perform a merge when a type exists nested under an
  alias of itself. Please separate them or delete one copy, and
  then try merging again.
```

``` ucm :hide
scratch/main> project.delete scratch
```

### Stray constructor alias

Constructors may only exist within the corresponding decl's namespace.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

Alice's branch:

``` ucm :hide
scratch/main> branch alice
```

``` ucm
scratch/alice> add

  ⍟ I've added these definitions:

    type Foo

scratch/alice> alias.term Foo.Bar AliasOutsideFooNamespace

  Done.
```

Bob's branch:

``` ucm :hide
scratch/main> branch bob
```

``` ucm
scratch/bob> add

  ⍟ I've added these definitions:

    bob : Nat
```

``` ucm :error
scratch/alice> merge bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Sorry, I wasn't able to perform the merge, because I need all
  constructor names to be nested somewhere beneath the
  corresponding type name.

  On scratch/alice, the constructor AliasOutsideFooNamespace is
  not nested beneath the corresponding type name. Please either
  use `move` to move it, or if it's an extra copy, you can
  simply `delete` it. Then try the merge again.
```

``` ucm :hide
scratch/main> project.delete scratch
```

### Term or type in `lib`

By convention, `lib` can only namespaces; each of these represents a library dependencies. Individual terms and types are not allowed at the top level of `lib`.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

Alice's branch:

``` ucm :hide
scratch/main> branch alice
```

``` unison :hide
lib.foo : Nat
lib.foo = 1
```

``` ucm :hide
scratch/alice> add

scratch/main> branch bob
```

Bob's branch:

``` unison :hide
bob : Nat
bob = 100
```

``` ucm :hide
scratch/bob> add
```

``` ucm :error
scratch/alice> merge /bob

  Loading branches...

  Sorry, I wasn't able to perform the merge:

  On scratch/alice, there's a type or term at the top level of
  the `lib` namespace, where I only expect to find subnamespaces
  representing library dependencies.

  Please move or remove it and then try merging again.
```

``` ucm :hide
scratch/main> project.delete scratch
```

## LCA precondition violations

The LCA is not subject to most precondition violations, which is good, because the user can't easily manipulate it\!

Here's an example. We'll delete a constructor name from the LCA and still be able to merge Alice and Bob's stuff
together.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

LCA:

``` unison
structural type Foo = Bar Nat | Baz Nat Nat
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      structural type Foo
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    structural type Foo

scratch/main> delete.term Foo.Baz

  Done.
```

Alice's branch:

``` ucm
scratch/main> branch alice

  Done. I've created the alice branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /alice`.

scratch/alice> delete.type Foo

  Done.

scratch/alice> delete.term Foo.Bar

  Done.
```

``` unison
alice : Nat
alice = 100
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      alice : Nat
```

``` ucm
scratch/alice> add

  ⍟ I've added these definitions:

    alice : Nat
```

Bob's branch:

``` ucm
scratch/main> branch bob

  Done. I've created the bob branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /bob`.

scratch/bob> delete.type Foo

  Done.

scratch/bob> delete.term Foo.Bar

  Done.
```

``` unison
bob : Nat
bob = 101
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      bob : Nat
```

``` ucm
scratch/bob> add

  ⍟ I've added these definitions:

    bob : Nat
```

Now we merge:

``` ucm
scratch/alice> merge /bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Loading dependents of changes...

  Loading and merging library dependencies...

  Rendering Unison file...

  Typechecking Unison file...

  I merged scratch/bob into scratch/alice.
```

``` ucm :hide
scratch/main> project.delete scratch
```

## Regression tests

### Delete one alias and update the other

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` unison
foo = 17
bar = 17
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      bar : Nat
      foo : Nat
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    bar : Nat
    foo : Nat

scratch/main> branch alice

  Done. I've created the alice branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /alice`.

scratch/alice> delete.term bar

  Done.
```

``` unison
foo = 18
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : Nat
```

``` ucm
scratch/alice> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> branch bob

  Done. I've created the bob branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /bob`.
```

``` unison
bob = 101
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      bob : Nat
```

``` ucm
scratch/bob> add

  ⍟ I've added these definitions:

    bob : Nat
```

``` ucm
scratch/alice> merge /bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Loading dependents of changes...

  Loading and merging library dependencies...

  Rendering Unison file...

  Typechecking Unison file...

  I merged scratch/bob into scratch/alice.
```

``` ucm :hide
scratch/main> project.delete scratch
```

### Delete a constructor

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` unison
type Foo = Bar | Baz
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type Foo
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    type Foo

scratch/main> branch topic

  Done. I've created the topic branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic`.
```

``` unison
boop = "boop"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      boop : Text
```

``` ucm
scratch/topic> add

  ⍟ I've added these definitions:

    boop : Text
```

``` unison
type Foo = Bar
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      type Foo
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` ucm
scratch/main> merge topic

  Loading branches...

  Loading definitions...

  Computing diffs...

  Loading dependents of changes...

  Loading and merging library dependencies...

  Rendering Unison file...

  Typechecking Unison file...

  I merged scratch/topic into scratch/main.

scratch/main> view Foo

  type Foo = Bar
```

``` ucm :hide
scratch/main> project.delete scratch
```

### Dependent that doesn't need to be in the file

This test demonstrates a bug.

``` ucm :hide
scratch/alice> builtins.mergeio lib.builtins
```

In the LCA, we have `foo` with dependent `bar`, and `baz`.

``` unison
foo : Nat
foo = 17

bar : Nat
bar = foo + foo

baz : Text
baz = "lca"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      bar : Nat
      baz : Text
      foo : Nat
```

``` ucm
scratch/alice> add

  ⍟ I've added these definitions:

    bar : Nat
    baz : Text
    foo : Nat

scratch/alice> branch bob

  Done. I've created the bob branch based off of alice.

  Tip: To merge your work back into the alice branch, first
       `switch /alice` then `merge /bob`.
```

On Bob, we update `baz` to "bob".

``` unison
baz : Text
baz = "bob"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      baz : Text
```

``` ucm
scratch/bob> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

On Alice, we update `baz` to "alice" (conflict), but also update `foo` (unconflicted), which propagates to `bar`.

``` unison
foo : Nat
foo = 18

baz : Text
baz = "alice"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      baz : Text
      foo : Nat
```

``` ucm
scratch/alice> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Everything typechecks, so I'm saving the results...

  Done.
```

When we try to merge Bob into Alice, we should see both versions of `baz`, with Alice's unconflicted `foo` and `bar` in
the underlying namespace.

``` ucm :error
scratch/alice> merge /bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Loading dependents of changes...

  Loading and merging library dependencies...

  Rendering Unison file...

  I couldn't automatically merge scratch/bob into scratch/alice.
  However, I've added the definitions that need attention to the
  top of scratch.u.

  When you're done, you can run

    merge.commit

  to merge your changes back into alice and delete the temporary
  branch. Or, if you decide to cancel the merge instead, you can
  run

    delete.branch /merge-bob-into-alice

  to delete the temporary branch and switch back to alice.
```

``` unison :added-by-ucm scratch.u
-- scratch/alice
baz : Text
baz = "alice"

-- scratch/bob
baz : Text
baz = "bob"

-- The definitions below are not conflicted, but they each depend on one or more
-- conflicted definitions above.

bar : Nat
bar =
  use Nat +
  foo + foo

```

But `bar` was put into the scratch file instead.

``` ucm :hide
scratch/main> project.delete scratch
```

### Merge loop test

This tests for regressions of https://github.com/unisonweb/unison/issues/1276 where trivial merges cause loops in the
history.

Let's make three identical namespaces with different histories:

``` unison
a = 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      a : ##Nat
```

``` ucm
scratch/alice> add

  ⍟ I've added these definitions:

    a : ##Nat
```

``` unison
b = 2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      b : ##Nat
```

``` ucm
scratch/alice> add

  ⍟ I've added these definitions:

    b : ##Nat
```

``` unison
b = 2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked the definitions in scratch.u. This
  file has been previously added to the codebase.
```

``` ucm
scratch/bob> add

  ⍟ I've added these definitions:

    b : ##Nat
```

``` unison
a = 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      a : ##Nat
```

``` ucm
scratch/bob> add

  ⍟ I've added these definitions:

    a : ##Nat
```

``` unison
a = 1
b = 2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked the definitions in scratch.u. This
  file has been previously added to the codebase.
```

``` ucm
scratch/carol> add

  ⍟ I've added these definitions:

    a : ##Nat
    b : ##Nat

scratch/bob> merge /alice

  Loading branches...

  Loading definitions...

  Computing diffs...

  Loading dependents of changes...

  Loading and merging library dependencies...

  Rendering Unison file...

  Typechecking Unison file...

  I merged scratch/alice into scratch/bob.

scratch/carol> merge /bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Loading dependents of changes...

  Loading and merging library dependencies...

  Rendering Unison file...

  Typechecking Unison file...

  I merged scratch/bob into scratch/carol.

scratch/carol> history

  Note: The most recent namespace hash is immediately below this
        message.



  This segment of history starts with a merge. Use
  `history #som3n4m3space` to view history starting from a given
  namespace hash.

  ⊙ 1. #b7fr6ifj87
  ⑃
  2. #9npggauqo9
  3. #dm4u1eokg1
```

``` ucm :hide
scratch/main> project.delete scratch
```

### Variables named `_`

This test demonstrates a change in syntactic hashing that fixed a bug due to auto-generated variable names for ignored
results.

``` ucm :hide
scratch/alice> builtins.mergeio lib.builtins
```

``` unison
ignore : a -> ()
ignore _ = ()

foo : Nat
foo = 18

bar : Nat
bar =
  ignore "hi"
  foo + foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      bar    : Nat
      foo    : Nat
      ignore : a -> ()
```

``` ucm
scratch/alice> add

  ⍟ I've added these definitions:

    bar    : Nat
    foo    : Nat
    ignore : a -> ()

scratch/alice> branch bob

  Done. I've created the bob branch based off of alice.

  Tip: To merge your work back into the alice branch, first
       `switch /alice` then `merge /bob`.
```

``` unison
bar : Nat
bar =
  ignore "hi"
  foo + foo + foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      bar : Nat
```

``` ucm
scratch/bob> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Previously, this update to `foo` would also cause a "real update" on `bar`, its dependent. Now it doesn't, so the merge
will succeed.

``` unison
foo : Nat
foo = 19
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : Nat
```

``` ucm
scratch/alice> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Everything typechecks, so I'm saving the results...

  Done.
```

``` ucm
scratch/alice> merge /bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Loading dependents of changes...

  Loading and merging library dependencies...

  Rendering Unison file...

  Typechecking Unison file...

  I merged scratch/bob into scratch/alice.
```

``` ucm :hide
scratch/main> project.delete scratch
```

### Unique type GUID reuse

Previously, a merge branch would not include any dependents in the namespace, but that resulted in dependent unique
types' GUIDs being regenerated.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` unison
type Foo = Lca
type Bar = MkBar Foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type Bar
      type Foo
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    type Bar
    type Foo

scratch/main> branch alice

  Done. I've created the alice branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /alice`.

scratch/alice> move.term Foo.Lca Foo.Alice

  Done.

scratch/main> branch bob

  Done. I've created the bob branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /bob`.

scratch/bob> move.term Foo.Lca Foo.Bob

  Done.
```

``` ucm :error
scratch/alice> merge /bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Loading dependents of changes...

  Loading and merging library dependencies...

  Rendering Unison file...

  I couldn't automatically merge scratch/bob into scratch/alice.
  However, I've added the definitions that need attention to the
  top of scratch.u.

  When you're done, you can run

    merge.commit

  to merge your changes back into alice and delete the temporary
  branch. Or, if you decide to cancel the merge instead, you can
  run

    delete.branch /merge-bob-into-alice

  to delete the temporary branch and switch back to alice.
```

``` unison :added-by-ucm scratch.u
-- scratch/alice
type Foo
  = Alice

-- scratch/bob
type Foo
  = Bob

-- The definitions below are not conflicted, but they each depend on one or more
-- conflicted definitions above.

type Bar
  = MkBar Foo

```

``` ucm
```

``` unison
type Foo = Merged
type Bar = MkBar Foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked the definitions in scratch.u. This
  file has been previously added to the codebase.
```

``` ucm
scratch/merge-bob-into-alice> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/merge-bob-into-alice> names Bar

  Type
  Hash:  #h3af39sae7
  Names: Bar

scratch/alice> names Bar

  Type
  Hash:  #h3af39sae7
  Names: Bar
```

``` ucm :hide
scratch/main> project.delete scratch
```

### Using Alice's names for Bob's things

Previously, we'd render Alice's stuff with her names and Bob's stuff with his. But because Alice is doing the merge,
we now use her names whenever possible. In this example, Alice calls something `foo` and Bob calls it `bar`. When
rendering conflicts, in Bob's term that references (what he calls) `bar`, we render `foo` instead.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` unison
hello = 17
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      hello : Nat
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    hello : Nat

scratch/main> branch alice

  Done. I've created the alice branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /alice`.
```

``` unison
hello = 18 + foo
foo = 100
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      foo : Nat
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      hello : Nat
```

``` ucm
scratch/alice> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> branch bob

  Done. I've created the bob branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /bob`.
```

``` unison
hello = 19 + bar
bar = 100
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      bar : Nat
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      hello : Nat
```

``` ucm
scratch/bob> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Note Bob's `hello` references `foo` (Alice's name), not `bar` (Bob's name).

``` ucm :error
scratch/alice> merge /bob

  Loading branches...

  Loading definitions...

  Computing diffs...

  Loading dependents of changes...

  Loading and merging library dependencies...

  Rendering Unison file...

  I couldn't automatically merge scratch/bob into scratch/alice.
  However, I've added the definitions that need attention to the
  top of scratch.u.

  When you're done, you can run

    merge.commit

  to merge your changes back into alice and delete the temporary
  branch. Or, if you decide to cancel the merge instead, you can
  run

    delete.branch /merge-bob-into-alice

  to delete the temporary branch and switch back to alice.
```

``` unison :added-by-ucm scratch.u
-- scratch/alice
hello : Nat
hello =
  use Nat +
  18 + foo

-- scratch/bob
hello : Nat
hello =
  use Nat +
  19 + foo

```

``` ucm :hide
scratch/main> project.delete scratch
```

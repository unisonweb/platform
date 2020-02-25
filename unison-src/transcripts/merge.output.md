
# How merging works

Suppose we have two branches, `P1` and `P2`, and a subnamespace, `foo`, which we'll refer to with `P1.foo` , `P2.foo`. This doc explains how `merge(P1,P2)` is computed, including the `merge(P1,P2).foo` subnamespace.

`LCA(P1,P2)` is the lowest common ancestor of `P1` and `P2`. To compute `merge(P1,P2)`, we:

1. Compute `LCA(P1,P2)` and do a three way merge of that level of the tree, using the algorithm below. What about the children of `P1` and `P2`? Let's just consider a child namespace `foo`. There are a few cases:
   1. `P1` and `P2` both have foo as a child namespace. Then `merge(P1,P2).foo == merge(P1.foo, P2.foo)`
   2. `P1` has `foo` as a child namespace, but `P2` does not (or vice versa). Then we have two subcases:
      1. `LCA(P1,P2)` has no `foo`. This means that `foo` child namespace was added by `P1`. The merged result for the `foo` subnamespace is just `P1.foo`.
      2. `LCA(P1,P2)` does have `foo`. This means that `P2` _deleted_ the `foo` subnamespace. The merged result for the `foo` subnamespace is then `merge(P1.foo, cons empty LCA(P1,P2).foo)`. This does a history-preserving delete of all the definitions that existed at the `LCA` point in history.
         1. Example is like if `P1` added a new definition `foo.bar = 23` after the `LCA`, then `foo.bar` will exist in the merged result, but all the definitions that existed in `foo` at the time of the `LCA` will be deleted in the result.

### Diff-based 3-way merge algorithm

Standard 3 way merge algorithm to merge `a` and `b`:

* Let `lca = LCA(a,b)`
* merged result is: `apply(diff(lca,a) <> diff(lca,b), lca)`

Relies on some diff combining operation `<>`.

```unison
foo.w = 2
foo.x = 1
baz.x = 3
quux.x = 4
```

```ucm
  ☝️  The namespace .P0 is empty.

.P0> add

  ⍟ I've added these definitions:
  
    baz.x  : Nat
    foo.w  : Nat
    foo.x  : Nat
    quux.x : Nat

```
Now P0 has 3 sub-namespaces.
* foo will be modified definition-wise in each branch
* baz will be deleted in the P2 branch and left alone in P1
* quux will be deleted in the P2 branch and added to in P1
* P1 will add a bar sub-namespace

```ucm
.P0> fork .P0 .P1

  Done.

.P0> fork .P0 .P2

  Done.

```
```unison
foo.y = 2483908
bar.y = 383
quux.y = 333
```

```ucm
.P1> add

  ⍟ I've added these definitions:
  
    bar.y  : Nat
    foo.y  : Nat
    quux.y : Nat

.P1> delete.term foo.w

  Name changes:
  
    Original       Changes
    1. P0.foo.w ┐  2. P1.foo.w (removed)
    3. P1.foo.w │  
    4. P2.foo.w ┘  
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
We added to `foo`, `bar` and `baz`, and deleted `foo.w`, which should stay deleted in the merge.

```unison
foo.z = +28348
```

```ucm
.P2> add

  ⍟ I've added these definitions:
  
    foo.z : Int

.P2> delete.namespace baz

  Removed definitions:
  
    1. x : Nat
  
  Tip: You can use `undo` or `reflog` to undo this change.

.P2> delete.namespace quux

  Removed definitions:
  
    1. x : Nat
  
  Tip: You can use `undo` or `reflog` to undo this change.

.P2> find

  1. foo.w : Nat
  2. foo.x : Nat
  3. foo.z : Int
  

```
We added `foo.z`, deleted whole namespaces `baz` and `quux` which should stay
deleted in the merge.

Now we'll try merging `P1` and `P2` back into `P0`. We should see the union of all their definitions in the merged version of `P0`.

This should succeed and the resulting P0 namespace should have `foo`, `bar`
and `quux` namespaces.

```ucm
.P0> merge .P1

  Here's what's changed in the current namespace after the
  merge:
  
  Added definitions:
  
    1. bar.y  : Nat
    2. foo.y  : Nat
    3. quux.y : Nat
  
  Removed definitions:
  
    4. foo.w : Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

.P0> merge .P2

  Here's what's changed in the current namespace after the
  merge:
  
  Added definitions:
  
    1. foo.z : Int
  
  Removed definitions:
  
    2. baz.x  : Nat
    3. quux.x : Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

.P0> find

  1. bar.y : Nat
  2. foo.x : Nat
  3. foo.y : Nat
  4. foo.z : Int
  5. quux.y : Nat
  

.P0> view foo.x foo.y foo.z bar.y quux.y

  bar.y : Nat
  bar.y = 383
  
  foo.x : Nat
  foo.x = 1
  
  foo.y : Nat
  foo.y = 2483908
  
  foo.z : Int
  foo.z = +28348
  
  quux.y : Nat
  quux.y = 333

```
These test that things we expect to be deleted are still deleted.

```ucm
.P0> view foo.w

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    foo.w

```
```ucm
.P0> view baz.x

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    baz.x

```
```ucm
.P0> view quux.x

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    quux.x

```

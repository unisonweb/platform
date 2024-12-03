<!-- https://github.com/unisonweb/unison/issues/4997 -->

# Delete namespace dependents check

This is a regression test, previously `delete.namespace` allowed a delete as long as the deletions had a name *anywhere* in your codebase, it should only check the current project branch.

``` ucm :hide
myproject/main> builtins.merge
```

``` unison
sub.dependency = 123

dependent = dependency + 99
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      dependent      : Nat
      sub.dependency : Nat
```

``` ucm :error
myproject/main> add

  ⍟ I've added these definitions:

    dependent      : Nat
    sub.dependency : Nat

myproject/main> branch /new

  Done. I've created the new branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /new`.

myproject/new> delete.namespace sub

  ⚠️

  I didn't delete the namespace because the following
  definitions are still in use.

  Dependency   Referenced In
  dependency   1. dependent

  If you want to proceed anyways and leave those definitions
  without names, use delete.namespace.force

myproject/new> view dependent

  dependent : Nat
  dependent =
    use Nat +
    dependency + 99
```

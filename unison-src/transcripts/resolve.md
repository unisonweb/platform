# Resolving edit conflicts in `ucm`

The `ucm` tool tracks edits to hashes in an object called a _patch_. When patches get merged, sometimes those patches will have conflicting edits. The `resolve.term` command helps resolve such conflicts.

First, let's make a new namespace, `example.resolve`:

```ucm
.> cd example.resolve
```

Now let's add a term named `a.foo`:

```unison
a.foo = 42
```

```ucm
.example.resolve> add
```

We'll fork the namespace `a` into a new namespace `b`, so we can edit the two concurrently.

```ucm
.example.resolve> fork a b
```

We'll also make a second fork `c` which we'll use as the target for our patch later.

```ucm
.example.resolve> fork a c
```

Now let's make a change to `foo` in the `a` namespace:

```ucm
.example.resolve> cd a
```

```unison
foo = 43
```

```ucm
.example.resolve.a> update
```

And make a different change in the `b` namespace:

```ucm
.example.resolve> cd .example.resolve.b
```

```unison
foo = 44
```

```ucm
.example.resolve.b> update
```

The `a` and `b` namespaces now each contain a patch named `patch`. We can view these:

```ucm
.example.resolve.b> cd .example.resolve
.example.resolve> view.patch a.patch
.example.resolve> view.patch b.patch
```

Let's now merge these namespaces into `c`:

```ucm
.example.resolve> merge a c
.example.resolve> merge b c
```

The namespace `c` now has an edit conflict, since the term `foo` was edited in two different ways.

```ucm
.example.resolve> cd c
.example.resolve.c> todo
```

We see that `#44954ulpdf` (the original hash of `a.foo`) got replaced with _both_ the `#8e68dvpr0a` and `#jdqoenu794`.

We can resolve this conflict by picking one of the terms as the "winner":

```ucm
.example.resolve.c> resolve.term #44954ulpdf #8e68dvpr0a
```

This changes the merged `c.patch` so that only the edit from #44954ulpdf to  #8e68dvpr0a remains:

```ucm
.example.resolve.c> view.patch
```

We still have a remaining _name conflict_ since it just so happened that both of the definitions in the edits were named `foo`.

```ucm
.example.resolve.c> todo
```

We can resolve the name conflict by deleting one of the names.

```ucm
.example.resolve.c> delete.term foo#jdqoenu794
```

And that's how you resolve edit conflicts with UCM.


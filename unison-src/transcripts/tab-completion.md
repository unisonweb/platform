# Tab Completion

Test that tab completion works as expected.

## Tab Complete Command Names

```ucm
.> debug.tab-complete vi
.> debug.tab-complete delete.
```

## Tab complete terms & types

```unison
subnamespace.someName = 1
subnamespace.someOtherName = 2
subnamespace2.thing = 3
othernamespace.someName = 4

unique type subnamespace.AType = A | B
```

```ucm:hide
.> add
```

```ucm
-- Should tab complete namespaces since they may contain terms/types
.> debug.tab-complete view sub
-- Should not complete things from child namespaces of the current query if there are other completions at this level
.> debug.tab-complete view subnamespace
-- Should complete things from child namespaces of the current query if it's dot-suffixed
.> debug.tab-complete view subnamespace.
-- Should complete things from child namespaces of the current query if there are no more completions at this level.
.> debug.tab-complete view subnamespace2
-- Should prefix-filter by query suffix
.> debug.tab-complete view subnamespace.some
.> debug.tab-complete view subnamespace.someOther
-- Should tab complete absolute names
.othernamespace> debug.tab-complete view .subnamespace.some
```

## Tab complete namespaces

```ucm
-- Should tab complete namespaces
.> debug.tab-complete find-in sub
.> debug.tab-complete find-in subnamespace
.> debug.tab-complete find-in subnamespace.
.> debug.tab-complete io.test sub
.> debug.tab-complete io.test subnamespace
.> debug.tab-complete io.test subnamespace.
```

Tab Complete Delete Subcommands

```unison
unique type Foo = A | B
add : a -> a
add b = b
```

```ucm
.> update
.> debug.tab-complete delete.type Foo
.> debug.tab-complete delete.term add
```

## Tab complete projects and branches

```ucm
myproject/main> branch mybranch
myproject/main> debug.tab-complete branch.delete /mybr
myproject/main> debug.tab-complete project.rename my
```

Commands which complete namespaces OR branches should list both

```unison
mybranchsubnamespace.term = 1
```


```ucm
myproject/main> add
myproject/main> debug.tab-complete merge mybr
```

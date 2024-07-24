The `switch` command switches to an existing project or branch.

```ucm:hide
foo/main> builtins.merge
bar/main> builtins.merge
```

Setup stuff.

```unison
someterm = 18
```

```ucm
foo/main> add
foo/main> branch bar
foo/main> branch topic
```

Now, the demo. When unambiguous, `switch` switches to either a project or a branch in the current project. A branch in
the current project can be preceded by a forward slash (which makes it unambiguous). A project can be followed by a
forward slash (which makes it unambiguous).

```ucm
scratch/main> switch foo
scratch/main> switch foo/topic
foo/main> switch topic
foo/main> switch /topic
foo/main> switch bar/
```

It's an error to try to switch to something ambiguous.

```ucm:error
foo/main> switch bar
```

It's an error to try to switch to something that doesn't exist, of course.

```ucm:error
scratch/main> switch foo/no-such-branch
```

```ucm:error
scratch/main> switch no-such-project
```

```ucm:error
foo/main> switch no-such-project-or-branch
```

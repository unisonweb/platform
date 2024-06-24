# Regression test for updates which conflict with an existing ability constructor

https://github.com/unisonweb/unison/issues/2786

```ucm:hide
.ns> builtins.merge
```

First we add an ability to the codebase.
Note that this will create the name `Channels.send` as an ability constructor.

```unison
unique ability Channels where
  send : a -> {Channels} ()
```

```ucm
.ns> add
```

Now we update the ability, changing the name of the constructor, _but_, we simultaneously
add a new top-level term with the same name as the constructor which is being
removed from Channels.

```unison
unique ability Channels where
  sends : [a] -> {Channels} ()

Channels.send : a -> ()
Channels.send a = ()

thing : '{Channels} ()
thing _ = send 1
```

We should be able to update everything at once.

```ucm
.ns> update
```

If `Channels.send` and `thing` _depend_ on `Channels`, updating them should succeed since it pulls in the ability as a dependency.

```unison
unique ability Channels where
  sends : [a] -> {Channels} ()

Channels.send : a -> ()
Channels.send a = sends [a]

thing : '{Channels} ()
thing _ = send 1
```

We should be able to successfully update the whole thing.

```ucm
.ns> update
```

# Constructor-term conflict

```ucm:hide
.ns2> builtins.merge
```


```unison
X.x = 1
```

```ucm
.ns2> add
```

```unison
structural ability X where
  x : ()
```

This should fail with a ctor/term conflict.

```ucm:error
.ns2> add
```

# `names` command

``` ucm
scratch/main> builtins.merge lib.builtins

  Done.
```

Example uses of the `names` command and output

``` unison
-- Some names with the same value
some.place.x = 1
some.otherplace.y = 1
some.otherplace.x = 10
somewhere.z = 1
-- Some similar name with a different value
somewhere.y = 2

another.Boolean = true

dd.baz = true
aa.baz = true
bb.baz = true
cc.baz = true

d.baz = 100
a.baz = 100
b.baz = 100
c.baz = 100

type a.baz = Boolean
type z.baz = Boolean


xyz.baz = 100.1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type a.baz
      type z.baz
      a.baz             : Nat
      aa.baz            : Boolean
      another.Boolean   : Boolean
      b.baz             : Nat
      bb.baz            : Boolean
      c.baz             : Nat
      cc.baz            : Boolean
      d.baz             : Nat
      dd.baz            : Boolean
      some.otherplace.x : Nat
      some.otherplace.y : Nat
      some.place.x      : Nat
      somewhere.y       : Nat
      somewhere.z       : Nat
      xyz.baz           : Float
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    type a.baz
    type z.baz
    a.baz             : Nat
    aa.baz            : Boolean
    another.Boolean   : Boolean
    b.baz             : Nat
    bb.baz            : Boolean
    c.baz             : Nat
    cc.baz            : Boolean
    d.baz             : Nat
    dd.baz            : Boolean
    some.otherplace.x : Nat
    some.otherplace.y : Nat
    some.place.x      : Nat
    somewhere.y       : Nat
    somewhere.z       : Nat
    xyz.baz           : Float
```

`names` searches relative to the current path.

``` ucm
-- We can search by suffix and find all definitions named 'x', and each of their aliases respectively.

scratch/main> names x

  'x':
  Hash          Kind   Names
  #pi25gcdv0o   Term   some.otherplace.x
  #gjmq673r1v   Term   some.otherplace.y,
                       some.place.x,
                       somewhere.z

-- We can search for multiple names in one command

scratch/main> names x y

  'x':
  Hash          Kind   Names
  #pi25gcdv0o   Term   some.otherplace.x
  #gjmq673r1v   Term   some.otherplace.y,
                       some.place.x,
                       somewhere.z

  'y':
  Hash          Kind   Names
  #gjmq673r1v   Term   some.otherplace.y,
                       some.place.x,
                       somewhere.z
  #dcgdua2lj6   Term   somewhere.y

-- We can search by hash, and see all aliases of that hash

scratch/main> names #gjmq673r1v

  '#gjmq673r1v':
  Hash          Kind   Names
  #gjmq673r1v   Term   some.otherplace.y,
                       some.place.x,
                       somewhere.z

-- Works with absolute names too

scratch/main> names .some.place.x

  '.some.place.x':
  Hash          Kind   Names
  #gjmq673r1v   Term   some.otherplace.y,
                       some.place.x,
                       somewhere.z
```

`debug.names.global` searches from the root, and absolutely qualifies results

``` ucm
-- We can search from a different branch and find all names in the codebase named 'x' and those named 'y', and each of their aliases respectively.

scratch/other> debug.names.global x y

  Found results in scratch/main

  'x':
  Hash          Kind   Names
  #pi25gcdv0o   Term   some.otherplace.x
  #gjmq673r1v   Term   some.otherplace.y,
                       some.place.x,
                       somewhere.z

  Found results in scratch/main

  'y':
  Hash          Kind   Names
  #gjmq673r1v   Term   some.otherplace.y,
                       some.place.x,
                       somewhere.z
  #dcgdua2lj6   Term   somewhere.y

-- We can search by hash, and see all aliases of that hash in the codebase

scratch/other> debug.names.global #gjmq673r1v

  Found results in scratch/main

  '#gjmq673r1v':
  Hash          Kind   Names
  #gjmq673r1v   Term   some.otherplace.y,
                       some.place.x,
                       somewhere.z

-- We can search using an absolute name

scratch/other> debug.names.global .some.place.x

  Found results in scratch/main

  '.some.place.x':
  Hash          Kind   Names
  #gjmq673r1v   Term   some.otherplace.y,
                       some.place.x,
                       somewhere.z
```

``` ucm :error
-- We can handle many name queries, some of which fail and some of which succeed

-- The names command is considered to have failed because there are 1 or more query failures

-- We can display hashes that are references to types and to terms

-- Each list of names in the Names column is sorted alphabetically

-- Each row is sorted by the Names column, alphabetically by name and then by the length of the list

scratch/main> names max /invalid1 /invalid2 + Boolean foo baz

  'max':
  Hash          Kind   Names
  ##Float.max   Term   lib.builtins.Float.max

  /invalid1:
  /invalid1 is not a well-formed name, hash, or hash-qualified
  name. I expected something like `foo`, `#abc123`, or
  `foo#abc123`.

  /invalid2:
  /invalid2 is not a well-formed name, hash, or hash-qualified
  name. I expected something like `foo`, `#abc123`, or
  `foo#abc123`.

  '+':
  Hash        Kind   Names
  ##Float.+   Term   lib.builtins.Float.+
  ##Int.+     Term   lib.builtins.Int.+
  ##Nat.+     Term   lib.builtins.Nat.+

  'Boolean':
  Hash            Kind   Names
  #idl63c82kf#0   Term   a.baz.Boolean
  #56fi1cmq3u     Term   aa.baz,
                         another.Boolean,
                         bb.baz,
                         cc.baz,
                         dd.baz
  ##Boolean       Type   lib.builtins.Boolean
  #cmihlkoddu#0   Term   z.baz.Boolean

  'foo':
  😶
  I couldn't find anything by that name.

  'baz':
  Hash          Kind   Names
  #idl63c82kf   Type   a.baz
  #u1qsl3nk5t   Term   a.baz, b.baz, c.baz, d.baz
  #56fi1cmq3u   Term   aa.baz,
                       another.Boolean,
                       bb.baz,
                       cc.baz,
                       dd.baz
  #00kr10tpqr   Term   xyz.baz
  #cmihlkoddu   Type   z.baz
```

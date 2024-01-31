```unison
{{ # Doc
This is a *doc*! 

term link {x}

type link {type   Optional}

}}
x : 
  Nat 
  -> Nat
x y =
    x   =     1 + 1
    x + y
-- Should keep comments after

-- Test for a previous regression that added extra brackets.
oneLiner = {{ one liner }}
-- After

-- Before
explicit.doc = {{
# Here's a top-level doc

With a paragraph

Or two
}}
-- After

{{ A doc before an ability }}
ability Thing where
  more  : Nat -> Text -> Nat
  doThing  : Nat -> Int

{{ 
A Doc before a type 
}}
structural type Optional   a = More Text 
  | Some 
  | Other   a 
  | None Nat 

{{ A doc before a type with no type-vars }}
type Two = One Nat | Two Text
```

```ucm
.> debug.format

```
```unison:added-by-ucm scratch.u
x.doc =
  {{
    # Doc This is a **doc**!
    
      term link {x}
      
      type link {type Optional}
  }}
x : Nat -> Nat
x y =
  use Nat +
  x = 1 + 1
  x + y
-- Should keep comments after

-- Test for a previous regression that added extra brackets.
oneLiner = {{ one liner }}
-- After

-- Before
explicit.doc =
  {{
    # Here's a top-level doc
    
      With a paragraph
      
      Or two
  }}
-- After

Thing.doc = {{ A doc before an ability }}
ability Thing where
  more : Nat -> Text ->{Thing} Nat
  doThing : Nat ->{Thing} Int

Optional.doc = {{ A Doc before a type }}
structural type Optional a = More Text | Some | Other a | None Nat 

Two.doc = {{ A doc before a type with no type-vars }}
type Two = One Nat | Two Text
```

Formatter should leave things alone if the file doesn't typecheck.

```unison
brokenDoc = {{ hello }} + 1
```

```ucm

  Loading changes detected in scratch.u.

  I couldn't figure out what + refers to here:
  
      1 | brokenDoc = {{ hello }} + 1
  
  The name + is ambiguous. I tried to resolve it by type but no
  term with that name would pass typechecking. I think its type
  should be:
  
      Doc2 -> Nat -> o
  
  If that's not what you expected, you may have a type error
  somewhere else in your code.
  Help me out by using a more specific name here or adding a
  type annotation.
  
  I found some terms in scope with matching names but different 
  types. If one of these is what you meant, try using its full 
  name:
  
  (Float.+) : Float -> Float -> Float
  (Int.+) : Int -> Int -> Int
  (Nat.+) : Nat -> Nat -> Nat

```
```ucm
.> debug.format

```

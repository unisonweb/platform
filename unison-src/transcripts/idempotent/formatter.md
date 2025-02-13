``` ucm :hide
scratch/main> builtins.mergeio
```

``` unison :hide
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

-- symbolyDefinition
(<|>) : Nat -> Nat -> (Nat, Nat)
(<|>) a b = (a, b)

symbolyEndOfBlock =
  x = 1
  (+:)


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


{{ Ability with single constructor }}
structural ability Ask a where
  ask : {Ask a} a

-- Regression test for: https://github.com/unisonweb/unison/issues/4666
provide : a -> '{Ask a} r -> r
provide a action =
  h = cases
        {ask -> resume} -> handle resume a with h
        {r} -> r
  handle !action with h

{{
A Doc before a type
}}
structural type Optional   a = More Text
  | Some
  | Other   a
  | None Nat

{{ A doc before a type with no type-vars }}
type Two = One Nat | Two Text

-- Regression for https://github.com/unisonweb/unison/issues/4669

multilineBold = {{

**This paragraph is really really really really really long and spans multiple lines
with a strike-through block**

_This paragraph is really really really really really long and spans multiple lines
with a strike-through block_

~This paragraph is really really really really really long and spans multiple lines
with a strike-through block~

}}
```

``` ucm
scratch/main> debug.format
```

``` unison :added-by-ucm scratch.u
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

-- symbolyDefinition
(<|>) : Nat -> Nat -> (Nat, Nat)
a <|> b = (a, b)

symbolyEndOfBlock =
  x = 1
  (+:)


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


Ask.doc = {{ Ability with single constructor }}
structural ability Ask a where
  ask : {Ask a} a

-- Regression test for: https://github.com/unisonweb/unison/issues/4666
provide : a -> '{Ask a} r -> r
provide a action =
  h = cases
    { ask -> resume } -> handle resume a with h
    { r }             -> r
  handle action() with h

Optional.doc = {{ A Doc before a type }}
structural type Optional a = More Text | Some | Other a | None Nat

Two.doc = {{ A doc before a type with no type-vars }}
type Two = One Nat | Two Text

-- Regression for https://github.com/unisonweb/unison/issues/4669

multilineBold =
  {{
  **This paragraph is really really really really really long and spans
  multiple lines with a strike-through block**
  
  __This paragraph is really really really really really long and spans
  multiple lines with a strike-through block__
  
  ~~This paragraph is really really really really really long and spans
  multiple lines with a strike-through block~~
  }}
```

Formatter should leave things alone if the file doesn't typecheck.

``` unison :error
brokenDoc = {{ hello }} + 1
```

``` ucm :added-by-ucm
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

``` ucm
scratch/main> debug.format
```

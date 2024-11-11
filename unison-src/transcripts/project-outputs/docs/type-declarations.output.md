draft draft draft

# Type Declarations in Unison

``` haskell
data DataDeclaration' v a = DataDeclaration {
  annotation :: a,
  bound :: [v],
  constructors' :: [(a, v, AnnotatedType v a)]
  -- isStructural :: IsStructural
  -- isOpaque :: Set (AnnotatedTerm v a)
} deriving (Eq, Show, Functor)

-- type IsStructural = Structural | Unique GUID
```

> There is some discussion history on this doc in the comment threads [here](https://github.com/unisonweb/unison/commit/bc65f460a7b6a6c0dec7f3028680d55f0372123e#comments) and [here](https://github.com/unisonweb/unison/commit/6be8cba7e7fde29cf87af7fb28f2b30185c40c89#commitcomment-33025457).

## Structural Types

> 👉 These got implemented - it's the default, so there's no `structural` keyword.

Structural types are defined uniquely by their structure. Every constructor has a unique signature, which intrinsically defines the meaning of the constructor. For example, the following types are identical and interoperable:

``` haskell
structural type Maybe a = Nothing | Just a
structural type Optional t = Some t | None
```

These definitions would also be identical and interoperable (although they maybe shouldn't be):

``` haskell
structural type Validation e a = Success a | Failure e
structural type Either a b = Left a | Right b
```

It should be an error if two constructors of a structural type have the same signature, indicating that the semantics are defined outside of the structure.

The identity of a structural type is determined by normalizing the constructor order by \<todo: explain how\> and then hashing their types.

## Unique types

> 👉 This got implemented - see [here](https://www.unison-lang.org/learn/language-reference/unique-types/).

Unique types have extrinsic semantics, not completely defined by the constructor types.  Their representation includes a GUID, along with the constructors.  The constructors types need not be unique.  The GUID is typically auto-generated, but can be specified as part of the type declaration, in order to use a textual representation to declare an identical type.

``` haskell
unique type Day = Mon | Tue | Wed | ...

unique[<guid>]
type Day = Mon | Tue | Wed | ...
```

Order of constructors having the same type is stable, but the relative constructor order of differently typed constructors is (currently) unspecified.

## Opaque Types

How do we support modularity?  That is, how do we let people expose a 'public API' to their library, and avoid exposing the internals behind it, so that (a) you can keep your library's internal data invariants intact without having to explain them, (b) you're free to change the internals without breaking client code that uses the API, and (c) you can tame complexity in the overall system by decoupling client code from library code?

The key thing is to control access to the introduction and elimination of data types: who is allowed to create, and to pattern-match on, a value of your type?  Both of those necessarily expose the guts of the representation of the type.

An opaque type has a structure and a block of terms that can inspect structure. The hash of those terms is part of the type ID.  They have a flag in the decl so typechecker can prevent access.

``` haskell
opaque type Socket = Socket Nat
opaque type Handle = Handle Text
```

Q: How do you declare a definition that can inspect two opaque types?
Q: How do *we* create and inspect Sockets?  We don't want to create public accessors, but we do want some way for privileged code to construct those values.  I guess it's straightforward for types with a single constructor, but we may end up needing some deterministic way of distinguishing the other constructors.

For reference and comparison: https://docs.scala-lang.org/sips/opaque-types.html
Notes re Scala opaque types:

  - They are a type alias (no boxing) that is only equal for definitions inside a corresponding companion object/module.
  - We (Unison) do need to "box" values within a constructor to give them a hash corresponding to their type identity.

### Alternative take on opaque types

The thread starting [here](https://unisonlanguage.slack.com/archives/CLKV43YE4/p1565135564409000) makes the case that it's not very 'open world' to force people to change your type's identity in order to add a function which is privileged - i.e. can create and pattern match on values of that type.

An alternative would be to say that, in terms of type identity, opaque types work exactly like unique types.  But that you can annotate terms as being a 'friend' of that type, and so allowed to create / pattern match.  So maybe here's what a term looks like that's a friend of types Foo and Bar:

``` haskell
friend[Foo, Bar] eg : Foo Bar
eg = Foo.Foo 1 "hi" (Bar.Bar 3.1)
-- syntax reminiscent of unique[#af361]
```

This annotation would be metadata attached to the term.  You can get unison to list all the friends of a given type, in order to work out what the footprint of 'privileged' code is.

### Private functions

It's not quite true to say that controlling creation and pattern matching is enough for the three aspects of modularity mentioned above.  What about internal library helper functions which could be called in a way that creates data that doesn't respect the invariants?  Or that you might want to change or remove later?  Or that are not at the same semantic level as your API?  So maybe we'd want a `private[Foo]` annotation on terms, which both implies `friend[Foo]`, and can only be referenced from other `friend[Foo]` terms.

## Combinations?

*Structural + Unique:* No.

*Structural + Opaque:* No.

*Unique + Opaque:* Sure why not.

(So note that Opaque implies Unique.)

Example where you want Opaque without Unique: `SortedSet` -- the exposed methods define the semantics.  Example where you want Unique + Opaque: `Socket`, `Handle` -- the exposed methods may necessarily dictate that the two types are not the same.

## Misc scenarios / questions:

I was just editing some Haskell code.

``` haskell
-- InputPatterns accept some fixed number of Required arguments of various
-- types, followed by a variable number of a single type of argument.
data IsOptional
  = Optional -- 0 or 1, at the end
  | Required -- 1, at the start
  | ZeroPlus -- 0 or more, at the end
  | OnePlus -- 1 or more, at the end
  deriving Show
```

I decided to move `Required` to the top for clarity since, as the comments state, InputPattern arg lists start with some number of `Required` arguments.

``` haskell
data IsOptional
  = Optional -- 0 or 1, at the end
  | Required -- 1, at the start
  | ZeroPlus -- 0 or more, at the end
  | OnePlus -- 1 or more, at the end
  deriving Show
```

I still want this to be the same type.  None of the semantics have changed, I just reordered the constructors for readability.  I don't think this would be possible with any of our current proposed type implementations.  Yes, I could create a new unique type, and refactor everything to use that, but that strikes me as unappealing, especially from a code-sharing perspective.

Thoughts?

  - @pchiusano - I'd say that "constructor display order" should be a bit of metadata that can be attached to a data declaration, and you should be able to edit this metadata somehow (perhaps by default, the `add` / `update` command can suggest "metadata edits" in reponse to this sort of thing).

## Old stuff: Algebraic Types?

Algebraic types are defined by their structure and a set of laws relating their fields.  Note that the laws may involve more than one type.

``` 
algebraic Monoid a = Monoid { mempty : a, mappend : a -> a -> a }
where m a -> (mappend m) (mempty m) a == a
      m a -> (mappend m) a (mempty m) == a
      m a b c -> (mappend m) a ((mappend m) b c) ==
                 (mappend m) ((mappend m) a b) c
```

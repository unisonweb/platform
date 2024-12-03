# Type declaration hashing and identity

This doc describes how data types are uniquely identified in Unison. There's been a bunch of discussion on this topic (todo - I can't seem to find the link to past discussions, help\!) but for v1 we'll keep it simple. We may add other ways of generating data type identities if/when we decide we really need it.

> 🚧 There's duplication between this doc and type-declarations.markdown ([github link](https://github.com/unisonweb/unison/blob/master/docs/type-declarations.markdown)).

**Background:** In most languages, a data type is uniquely identified by some named type within some package. If either the package name is changed (due to a new numbered release of the package) or the module name or name of the type is changed, this results in a type that the language type system considers to be different.

In Unison, a type declaration (introduced by either the `type` or `ability` keyword) creates a type which is uniquely identified in one of two ways:

  - A *structural* type declaration is identified by a hash of its structure, exactly as is done for hashing of a term. This is the current default if you just write: `type Blah = ...`.
  - A *nominal* type declaration is identified by a GUID generated at the time the declaration. Syntax for this is TBD, but perhaps: `nominal type Blah = ...`

Notes:

  - Structural types have unordered constructors, and their identity isn't affected by the names chosen, so `type O a = N | S a`  is the same type as `type Maybe a = Just a | Nothing`.
  - If the user writes a structural type where two constructors have the same structure, that's a type error and the user should be prompted to either make the structure different or choose a different.
  - Nominal types have ordered constructors. The order of the constructors is frozen at the time of the creation of the type. The constructors and the type may be renamed, but the GUID associated with the type never changes.

Nominal types are to be used for things like "days of the week". Structural types are to be used for things like `List` or `Maybe`.

That's it for now.

## Other ideas and notes

Possibly for later:

  - *opaque/whatever* - a newtype with some privileged functions that can treat it as a type alias instead of newtype
  - *algebraic* - defined by a set of laws (Monoid, Semilattice, etc) Question around how those laws are encoded

Other notes:

  - Want a nice story for refactoring: e.g. if I have a conversion from T1 to T2, that can be applied automatically everywhere T1 is in positive position. T2 -\> T1 will cover where T1 is in negative position; isomorphism will cover both.
  - Want a nice story for discovery of existing types to limit fragmentation.

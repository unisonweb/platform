This document explains how to add builtins to the language by working
through the example of adding `MVar` and some associated functions.

## Builtin Data

The logical first step for this example is to add a built-in `MVar`
type, whose values will simply be wrapped values of the Haskell type
with the same name. The 'old' runtime deviates from this approach for
several types, but this is how e.g. `Text` works even there.

Data types, including opaque pseudo data types of this sort are
referred to by `Reference`. Builtin, opaque data types use the
`Builtin` constructor with an appropriate name. The ones in actual
use are listed in the `Unison.Type` module, so we'll add a definition
there:

``` haskell
mvarRef :: Reference
mvarRef = Reference.Builtin "MVar"
```

This definition alone won't do anything, however. It is merely
something for other definitions to refer to. If the reference is used
in e.g. the type of a function definitions without giving it an actual
name in the codebase, the type will be displayed with the raw hash,
which looks like `#MVar`.

The builtin reference can be given a name during the `builtins.merge`
ucm command. To make this happen, we must modify the `builtinTypesSrc`
definition in the `Unison.Builtin` module. This is just a list of
values that describe various builtin type related actions to be
performed during that command. In this case, we will add two values to
the list:

``` haskell
B' "MVar" CT.Data
```

This specifies that there should be a builtin data type referring to
the `Builtin "MVar"` reference. The codebase name assigned to this is
the same as the reference (MVar here), but nested in the `builtin`
namespace. However, we will also add the value:

``` haskell
Rename' "MVar" "io2.MVar"
```

because this is a type to be used with the new IO functions, which are
currently nested under the `io2` namespace. With both of these added
to the list, running `builtins.merge` should have a `builtin.io2.MVar`
type referring to the `Builtin "MVar"` reference.

The reason for both a `B'` and a `Rename'` is that eventually one
would expect the IO functionality to be moved from the `io2`
namespace. However, the builtin reference name may not be changed
easily, so it is preferable to have it named in the eventual expected
way, rather than permanently named `io2.MVar` internally.

## Builtin function declarations

The next step is to declare builtin functions that make use of the new
type. These are declared in a similar way to the type names above.
There is another list in `Unison.Builtin`, `builtinsSrc`, that defines
values specifying what builtin functions should exist.

Like the builtin type list, there are declarations for adding a
builtin function with a given name, and declarations for renaming from
the given name to a different namespace location. For the `MVar`
functions, we'll again give them their intended names as the original,
and rename them to the `io2` namespace for the time being.

Builtin functions also have an associated type as part of the initial
declaration. So for the complete specification of a function, we will
add declarations similar to:

``` haskell
B "MVar.new" $ forall1 "a" (\a -> a --> io (mvar a))
Rename "MVar.new" "io2.MVar.new"
B "MVar.take" $ forall1 "a" (\a -> mvar a --> iof a)
Rename "MVar.take" "io2.MVar.take"
```

The `forall1`, `io`, `iof` and `-->` functions are local definitions
in `Unison.Builtin` for assistance in writing the types. `iof`
indicates that an error result may be returned, while `io` should
always succeed. Note that when the `{IO}` ability appears as a type
parameter rather than the return type of a function, you will need to
use `iot` instead.
`mvar` can be defined locally using some other
helpers in scope:

``` haskell
mvar :: Type -> Type
mvar a = Type.ref () Type.mvarRef `app` a
```

For the actual `MVar` implementation, we'll be doing many definitions
followed by renames, so it'll be factored into a list of the name and
type, and we can then call the `moveUnder` helper to generate the `B`
declaration and the `Rename`.

## Builtin function implementation -- new runtime

What we have done so far only declares the functions and their types.
There is nothing yet implementing them. This section will proceed
through the implementation backing the declarations of the `MVar.new`
and `MVar.take` above.

In this case, we will implement the operations using the 'foreign
function' machinery. This path is somewhat less optimized, but
doesn't require inventing opcodes and modifying the runtime at
quite as low a level. The builtin 'foreign' functions are declared
in `Unison.Runtime.Builtin`, in a definition `declareForeigns`. We
can declare our builtins there by adding:

``` haskell
  declareForeign Tracked "MVar.new" boxDirect
    . mkForeign $ \(c :: Closure) -> newMVar c
  declareForeign Tracked "MVar.take" boxToEFBox
    . mkForeignIOF $ \(mv :: MVar Closure) -> takeMVar mv
```

These lines do multiple things at once. The first argument to
`declareForeign` determines whether the function should be explicitly
tracked by the Unison Cloud sandboxing functionality or not. As a
general guideline, functions in `{IO}` are `Tracked`, and pure
functions are `Untracked`. The second argument must match the name
from `Unison.Builtin`, as this is how they are associated. The third
argument is wrapper code that defines the conversion from the Haskell
runtim calling convention into Unison, and the definitions for these
two cases will be shown later. The last argument is the actual Haskell
implementation of the operation. However, the format for foreign
functions is somewhat more limited than 'any Haskell function,' so the
`mkForeign` and `mkForeignIOF` helpers assist in wrapping Haskell
functions correctly. The latter will catch some exceptions and yield
them as explicit results.

The wrapper code for these two operations looks like:

``` haskell
-- a -> b
boxDirect :: ForeignOp
boxDirect instr =
  ([BX],)
    . TAbs arg
    $ TFOp instr [arg]
  where
    arg = fresh1

-- a -> Either Failure b
boxToEFBox :: ForeignOp
boxToEFBox =
  inBx arg result $
    outIoFailBox stack1 stack2 stack3 any fail result
  where
    (arg, result, stack1, stack2, stack3, any, fail) = fresh
```

The breakdown of what is happening here is as follows:

  - `instr` is an identifier that is used to decouple the wrapper
    code from the actual Haskell implementation functions. It is
    made up in `declareForeign` and passed to the wrapper to use as a
    sort of instruction code.
  - A `ForeignOp` may take many arguments, and the list in the tuple
    section specifies the calling convention for them. `[BX]` means
    one boxed argument, which in this case is the value of type `a`.
    `[BX,BX]` would be two boxed arguments, and `[BX,UN]` would be
    one boxed and one unboxed argument. Builtin wrappers will
    currently be taking all boxed arguments, because there is no way
    to talk about unboxed values in the surface syntax where they are
    called.
  - `TAbs arg` abstracts the argument variable, which we got from
    `fresh1'` at the bottom. Multiple arguments may be abstracted with
    e.g. `TAbss [x,y,z]`. You can call `fresh` to instantiate a tuple of
    fresh variables of a certain arity.
  - `inBx` and `outIoFailBox` are helper functions for calling the
    instruction and wrapping up a possible error result.
  - `TFOp` simply calls the instruction with the assumption that the
    result value is acceptable for directly returning. `MVar` values
    will be represented directly by their Haskell values wrapped into
    a closure, so the `boxDirect` code doesn't need to do any
    processing of the results of its foreign function.

The names of the helpers generally follow a form of form of Hungarian
notation, e.g. `boxToEFBox` means "boxed value to either a failure or
a boxed value", i.e. `a -> Either a b`.
However, not all helpers are named consistently at the moment, and
different builtins use slightly different implementations, so looking
at other parts of the file may be instructive, depending on what is
being added.

At first, our declarations will cause an error, because some of the
automatic machinery for creating builtin 'foreign' functions does not
exist for `MVar`. To rectify this, we can add a `ForeignConvention`
instance in `Unison.Runtime.Foreign.Function` that specifies how to
automatically marshal `MVar Closure`, which is the representation
we'll be using.

``` haskell
instance ForeignConvention (MVar Closure) where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  writeForeign = writeForeignAs (Foreign . Wrap mvarRef)
```

This takes advantage of the `Closure` instance, and uses helper
functions that apply (un)wrappers from another convention.

With these in place, the functions should now be usable in the new
runtime.

## Decompilation

If it makes sense for an added type, it is possible to add to Unison's
ability to decompile runtime values or test for universal
equality/ordering. Directly embedded Haskell types are wrapped in the
`Foreign` type, and are decompiled in `Unison.Runtime.Decompile` using
the `decompileForeign` function. For instance, `Text` is decompiled in
the case:

``` haskell
  | Just t <- maybeUnwrapBuiltin f = Right $ text () t
```

Further cases may be added using the `maybeUnwrapBuiltin`, which just
requires adding an instance to the `BuiltinForeign` class in
`Unison.Runtime.Foreign`, specifying which builtin reference
corresponds to the type.

## Transcripts

One last thing remains. The additional builtin operations will have
changed some of the transcript output. The transcript runner should be
executed, and modified files should be checked and committed, so that
CI tests will pass (which check transcripts against an expected
result).

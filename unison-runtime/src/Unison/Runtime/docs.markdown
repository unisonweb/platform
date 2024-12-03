
# Documentation of the Unison runtime

This directory has the code for the Unison runtime.

The Unison runtime is responsible for evaluating Unison code which has been parsed and typechecked. Evaluation converts _computations_, which contain reducible expressions (an expression like `1 + 1` or `case (a,b) of ..`) to _values_ (like `42`), which don't have redexes and which are said to be in _normal form_. The runtime has some design constraints:

##### It should be possible at runtime to hash, serialize, deserialize, and compute the dependencies of any value in the language, including functions.

These capabilities are needed for the implementation of Unison's distributed programming API which ships arbitrary values over the network (and these functions are also just super convenient for other reasons too). That is, it needs to be possible to have functions like:

  * `encode : forall a . a -> Bytes`
  * `decode : forall a . Bytes -> Either Err a`
  * `dependencies : forall a . a -> Set Reference`
  * `hash : forall a . a -> Hash`
  * Note: the types of these might be more constrained than this so you can't break parametricity and use them in parametric code, but the idea is that they could in principle have these types and they really do work _for all_ `a`.

Importantly, values may contain cycles (a recursive function, for instance), and these to be serializable and hashable as well, so there needs to be a way of detecting and encoding these cycles reliably in all cases (having the serialization code blow up when it hits a cycle is not allowed).

##### The runtime should make it possible to decompile any value back to a Unison term.

When you evaluate a watch expression, you should see a term in normal form. This is nicer than the usual approach of having runtime values be their own universe and requiring the programmer to write a bunch of boilerplate to extract useful information from these runtime values.

##### The runtime should support algebraic effects, which requires being able to manipulate continuations of a running program.


##### This first version of the Haskell runtime isn't aiming for extreme speed. It should be correct, simple, and easy for us to understand and maintain.

Within these parameters, if there's easy speed improvements to be had, great. And perhaps later, we can have a more complicated but insanely fast runtime which is also correct because of intense engineering effort. But now is not the time for that.

##### The runtime should be modular, so that pieces of it can be reused even if we move from, say, directly interpreting some instruction set to JIT-ing via LLVM.

The old Scala runtime was monolithic, going directly from a term to a compiled form.

## Overview

To evaluate a Unison term, `p : AnnotatedTerm v a` which has been successfully typechecked, the runtime goes through several phases:

    p0 : AnnotatedTerm v a
             ||
         let rec minimization
             ||
    p1 : AnnotatedTerm v a
             ||
         lambda lifting
             ||
    p2 : AnnotatedTerm v a
             ||
         A-normal form (ANF) conversion
             ||
    p3 : AnnotatedTerm v a
             ||
         compilation
             ||
    p4 : IR (see `IR.hs`)
             ||
         evaluation
             ||
    p5 : IR.V (see `IR.hs`)
             ||
         decompilation
             ||
    p6 : AnnotatedTerm v ()

Here's a brief overview of these phases:

* let rec minimization eliminates needless cycles and reduces cycle sizes to the minimum, to prepare for just having `let` be the only place in the runtime that must deal with ability requests.
* lambda lifting eliminates lambdas with free variables by adding extra parameters for each free variable, simplifying the later compilation phase.
* ANF moves any function calls or ability requests to the body of a `let` or `let rec`, which is the last thing needed to ensure that `let` is the only place we need to deal with ability requests.
* Compilation converts the ANF code to an intermediate representation, which can be interpreted directly to produce a value, `V`.
* After evaluation, the `V` can be decompiled back to a term, which can be displayed to the user in the codebase editor tool.

#### Phase 1: let rec minimization

_let rec minimization_ breaks up recursive blocks into a mix of `let` and minimally-sized `let rec` blocks. The implementation is in [`Unison.Typechecker.Components.minimize`](../Typechecker/Components.hs#L17).

__Why do we do this?__

* We decided for sanity and simplicity that the bindings of a cycle (like `ping` and `pong`) can't use any abilities, since it's unclear what order things happen in (if `ping` uses abilities and has a forward reference to `pong`, and `pong` has a reference to `ping` and uses abilities, which effect should happen first??). To clarify, mutually recursive functions in a let rec may use abilities in their body, since those abilities aren't required until the function is called.
* But when the source of a program reveals a clear dependency order to the bindings, we want to be able to use abilities.
* This transformation is also useful in conjunction with ANF conversion - it means that interpretation of `let` is the _one place in the runtime_ where we need to expect an ability request. It makes it very easy to construct the continuations which are passed to the ability handlers.

_Note:_ The typechecker also does this same let rec minimization transform before typechecking, and when typechecking any `let rec` blocks that remain, it sets the ambient ability to be empty. (TODO: check that typechecker does in fact do this, and fix if not)

#### Phase 2: lambda lifting

This transform is currently in the [`ANF.hs`](ANF.hs#L26) file, see the `lambdaLift` function there. This transform eliminates free variables from any lambdas in the term, by turning them into ordinary function parameters. This leaves only closed lambdas, which are easy to compile.

A lambda with free variables is really a program that will generate a function at runtime when values for those free variables are known. Turning these free variables into function parameters just means less cases to deal with later on during compilation.

#### Phase 3: ANF conversion

See [Wikipedia page for ANF](https://en.wikipedia.org/wiki/A-normal_form). __Why do we do this?__ It's much simpler to compile and optimize, and importantly, it leaves us with __just one place__, in `let`, where the continuations of ability requests are processed by the runtime.

Example:

```
handle (state 0) in
  x = State.get + 3
  y = x + 1
  State.set 42
  99
```

This isn't in ANF, and if we tried to compile this directly, our code for doing function application (the `State.get + 3`) would need to be prepared to deal with ability requests and would need to be able to construct the appropriate continuation:

```
r -> let
  x = r + 3
  y = x + 1
  State.set 42
  99
```

In contrast, if the code is in ANF, then function application doesn't need to deal with ability requests, as functions are always being applied to values:

```
handle (state 0) in
  r = State.get
  x = r + 3
  y = x + 1
  State.set 42
  99
```

#### Phase 4: compilation to IR

The `IR` type, defined in `IR.hs`, denotes a `[Value] -> Result Value`, a function that takes a stack of values and returns a result, where:

```
type Result v =
  MatchFail | Done v |
  Request Reference CtorId [v]  IR
--        ^         ^      ^    ^
--        ability   ctor   args continuation
```

`Value` (defined in [`IR.hs`](Value.hs)) has no redexes and is in normal form.

An example of `Request`, for the expression `State.set 42` - the `Reference` is `State`, the `CtorId` (an `Int`) is the constructor number for `State.set`, the args is `[42]`, and

`IR` is represented as an ANF tree, where variable references are represented by their [De Bruijn index](https://en.wikipedia.org/wiki/De_Bruijn_index): the nearest / innermost bound variable has an index of 0, the next nearest has an index of 1, etc. Some more interesting examples:

* In `x y -> x + y`, in the body of the lambda, `x` has an index of 1 and `y` has an index of 0.
* In `let x = 1; y = x + 1`, in the body of `y`, `x` has an index of 0.
* In `let rec ping x = pong x; pong x = ping x`, in the body of `ping`, `x` has an index of `0`, `pong` has an index of `1`, and `ping` has an index of `2`.
* In `case x of (y, (z, q)) -> y + z`, in the `y + z` expression, `y` has an index of `2` and `z` has an index of `1` and `q` has an index of 0.

Put another way, variable bindings are pushed onto a stack in "source order", and their position on the stack at each usage site is their De Bruijn index.

In order to convert from the ANF form of the term, which has named variable references, and the De Bruijn indexed `IR` type, we need to convert from named references to De Bruijn indices. For that, we use the function [`ABT.annotateBound'`](../ABT.hs#L120-L126), which is defined for any abstract binding tree:

```haskell
annotateBound'
  :: (Ord v, Functor f, Foldable f)
  => Term f v a0
  -> Term f v [v]
```

This annotates every node of an ABT with the list of bound variables, in order, such that the de bruijn index of variable, `v`, can be computed via [`elemIndex`](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html#v:elemIndex) of `v` in this list. Easy peasy.

Once we have that, the conversion to `IR`, defined in `IR.compile0`, is straightforward. A couple wrinkles:

* When a lambda is partially applied, for instance `(x y -> foo x + y) 42`, we recompile the lambda, subsituting the arguments already applied, in this case substituting `x` with `42`, leaving the lambda `(y -> foo 42 + y)`. To support this, the `compile0` function actually takes this environment of already evaluated values, as a `[(SymbolC, Maybe V)]`. This is added onto the end of the list of bound variables.
* Variables which are lambda-lifted out of a function are compiled as "lazy stack lookups" (the `LazySlot` constructor, defined in [`IR.hs`](IR.hs)), which doesn't look inside references on the stack. Why is this done?

Well, consider:

```Haskell
let rec loop = n ->
  if n == 100 then n
  else loop (n + 1)
```

After lambda lifting, this looks like:

```Haskell
let rec loop =
  (self n -> if n == 100 then n
             else self (n + 1)) loop
                                ^^^^
```

But this isn't quite right - the `loop` which is passed to itself needs to be passed lazily, otherwise this would not terminate. Really, we want something more like:

```Haskell
let rec loop =
  (self n -> if n == 100 then n
             else !self (n + 1)) 'loop
                                  ^^^^
```

Notice that `loop` is passed as the thunk, `'loop`, which is forced inside the body (the `!self`). But we don't literally need to use thunks like this, we can just avoid forcing the reference which appears at that stack slot.

The `SymbolC` variable type used by the `compile0` function just tracks which variables need to get this treatment - these variables are compiled as a `LazySlot` rather than a `Slot`.

#### Phase 5: evaluation

There are interpreters in [`Rt0.hs`](Rt0.hs) and (in progress) [`Rt1.hs`](Rt1.hs). Recall our denotation for `IR` is an `[Value] -> Result Value`, a function that takes a stack of values and produces a `Result`, where:

```
type Result v =
  MatchFail | Done v |
  Request Reference CtorId [v]  IR
--        ^         ^      ^    ^
--        ability   ctor   args continuation
```

If you go through [`Rt0.hs`](Rt0.hs), many of the cases are straightforward: there is basically only one or two reasonable things to do. In general, instructions that introduce variables (like `let`, `let rec`, function calls, and pattern matching) will push onto the stack and invoke the interpreter on subexpressions with this updated stack.

As a result of the ANF representation, only `let` needs to be prepared to deal with a `Request` result.

A couple wrinkles:

* Functions, when fully applied, are simple to interpret: we push the arguments onto the stack and then run the body of the function. But functions can also be under-applied (given fewer arguments than their arity) or over-applied (given more arguments than their arity). These cases all need to be handled:
  * Under-applied functions are recompiled, with the given arguments substituted into the body of the function. So `(x y -> x + y) 42` becomes the lambda `y -> 42 + y`.
  * Over-applied functions are treated as a fully-applied function, and the result of that call is then applied to the remaining arguments. So `(x -> x) (y -> y) 42` first evaluates the `(x -> x) (y -> y)`, producing `(y -> y)`, and then applies `(y -> y) 42`, producing `42`.
* __Tail calls:__ When a function call is the last expression in a function, that call can discard the stack elements for the current call.
  * That is, suppose we are inside the function `x y -> let z = x + y; foo z 19`. At the point of the `foo z 19` call, `x` and `y` are on the stack, but aren't needed anymore. The call to `foo` could therefore drop `x` and `y` from the stack, push `z` and `19` onto the stack and then call `foo`. `foo` only examines at most two stack elements, one for each arg it receives, so it doesn't care whether `x` and `y` are below it in the stack or not.
  * There are a few approaches for allowing the stack to be reclaimed:
    * One is to have a separate tail call instruction in the `IR` (this makes the `compile` function a bit more complicated, and the evaluator, since it needs to different cases, one for regular calls and one for tail calls).
    * Another is to track in the evaluator when a function call is in tail position, and interpret function calls accordingly (this means more state being tracked by the evaluator, and more cases.
    * The last approach is to just garbage collect the stack occasionally, this is Baker's idea, [Cheney on the MTA](http://home.pipeline.com/~hbaker1/CheneyMTA.html). To GC the stack, you simply look at the current `IR` and compute its max De Bruijn index, say that's 4, which means that only the top `5` elements of the stack are still referenced by the rest of your computation. You copy these 5 elements to a fresh stack, preserving their order, reset the top of the stack to `5`, and continue on.
    * And these approaches need not be mutually exclusive - you can garbage collect the stack and stil have a separate tail call instruction.

Currently, the `IR` doesn't have a separate tail call instruction and nothing is implemented for tail calls. I think the Cheney on the MTA is very simple, so will probably just do that for now.

One interesting aspect of the Cheney on the MTA approach is that it's more accurate about garbage collecting references that remain in lexical scope, but which aren't used any longer. For instance, consider:

```Haskell
let
  x = computeHugeList 99
  n = Sequence.size x
  y = loop n
  y + 100
```

Assume `x` is some huge list and `loop` is some long running loop. At the point where this `loop` function is invoked, `x` is no longer used by the rest of the computation, but because `loop` isn't a tail call, `x` is kept around on the stack and not GC'd. With the Cheny on the MTA approach, this doesn't matter--`x` can be garbage collected as soon as the continuation of the computation no longer references it, independent of any tail calls.

It seems nice to do this sort of GC (possibly in addition to having a separate tail call instruction). Having some dangling reference in lexical scope is one of those things that causes occasional hard-to-debug memory leaks. I've heard that the JVM will even null out stack slots which aren't used anymore, which is a bit like this.

# Distributed programming API v1 discussion

``` haskell
type Either a b = Left a | Right b
type Status = Running | Finished | Canceled | Error Error
type Error =
	Unknown | Unreachable | Unresponsive | AbilityCheckFailure

ability Remote location where
  fork   : location {e} -> '{e} a ->{Remote location} Future a
  join   : Future a ->{Remote location} Either Error a
  status : Future a ->{Remote location} Status
  cancel : Future a ->{Remote location} Either Error ()

type Future a = Future
  ('{Remote loc} (Either Err a) -- join
  ,'{Remote loc} ()             -- cancel
  ,'{Remote loc} Status,        -- status
  , Duration ->{Remote loc} ())  -- keepalive (seconds 10)
```

Feb 11 Q\&A:

  - Do we need `Remote.here`? Thinking is: we don’t, we can just get one when starting the Unison Remote server; can then use that value, or restricted derivatives, in applications.

``` 
Unison.server
  -> (Location {e} ->{Remote Location} r) -- local computation
  -> {e} r                                -- rrrrresult
```

`handle expression with handler`  OR  
`with handler handle expression`
\* How do you launch anything?
\* Watch expression lol
\* launch

  - What does it mean to `cancel`?  
    Proposal: Runtime needs to support this. `fork`-ing in Unison likely works by forking a new instance `t` of Haskell runtime; that Haskell thread `t` can be asynchronously interrupted.  So, the implementation of `Future.cancel` just throws a Haskell async exception into `t`, terminating that instance of the runtime.

  - How do decide if a received computation is allowed to be run? (and we are capable of running it?)
    
    1.  Some Unison term comes over the wire.
    2.  \-Decide the type (typecheck?  maybe slow? some other proof?)- No, we can use runtime exception.
    3.  Scan the term for unknown hashes. (Could we do this lazily?  Arya says: that’s crazay \[sic\]\!  Rúnar adds:  Sounds super fragile.)
          - Could speculatively send some dependencies with the initial request, especially if protocol has minimum message size, but maybe not easy to anticipate which dependencies will be needed at remote end.
          - If doing this lazily, could spare sending definitions for code paths not used during this particular execution.
          - Could get started running the computation if there’s any work that can be done before receiving missing dependencies.  Background thread works to populate the term cache from remote sources.
    4.  If missing some of the dependencies, send list of references back to originator for definitions.  Repeat steps 3–4 until the whole application is loaded / cached / whatever.
    5.  Just run it and then complain if encountering an unexpected ability request.

  - How do actually run one?

-----

Do we need to choose a representation of `Location` now?

  - No, we can use incrementally more sophisticated representations. e.g., loc can initially be `()` or `Nat`, and the handler can maintain pure maps or whatever.  (note: need pure maps).
  - Yes, because the entire `Remote` ability needs to be defined up front, but some APIs e.g. relating to “keepalives” only make sense in the context of true multi-node Locations.

Do we need to choose a representation of `Future` now?

  - Yes, because the entire `Remote` ability needs to be defined up front, but we may need additional remote abilities to operate on `Future`s.
  - It can just be `'{Remote loc} a`
      - No, this representation doesn’t contain enough info to asynchronously identify the computation, e.g. to support `Remote.status` in a multi-node implementation.
  - It can be some kind of handle or GUID.
      - Can we index typed results by untyped handle?

Do we need the ability to automatically clean up zombie tasks?  This informs the discussion around keepalives.

  - Yes:

## Locations

A Location is simply a computing context with access to certain computational resources.  The `Remote` ability is parameterized with a Location type `loc`, giving us significant flexibility in defining various `Remote` interpreters.  The interpreter can then require a `loc` that describes resources in whatever way it likes, and the interpreter can be paired with an appropriate implementation for obtaining or generating `loc`s.

For example:

``` haskell
runLocal : '{Remote () ()} a -> a
runLocal r =
  step nid r = case r of
    {a} -> a
    {Remote.fork t -> k} -> handle (step nid) in k t
    {Remote.spawn -> k} -> handle (step (Node.increment nid)) in k nid
    {Remote.at _ t -> k} -> handle (step nid) in k !t
  handle (step (Node.Node 0)) in !r
```

Its runtime representation is essentially a collection of cryptographic tokens authorizing the use of these resources.

In Unison code, a Location is represented by a `Loc {e}`. A Unison value of type `Loc {}` supports only pure computations, whereas a `Loc {Remote, GPU}` provides the `Remote` and `GPU` abilities.

### Locations have a composite runtime representation

A `Loc` is represented by one or more host / port / auth tokens, along with ability use tokens.  The `Remote` handler may use any algorithm in selecting a host to submit a task to, and the receiving host will run the computation provided the accompanying tokens are valid.

``` haskell
-- Haskell runtime representation
-- individual Tokens should be cryptographically unguessable.
-- Tokens may correspond to or contain quota/other data.
data Loc = Loc Hosts Abilities
type Token = TBD
type Hosts = Map (Hostname,Port) Token
type Abilities = Map Reference Token -- Map Reference (PublicKey, RandomDigits, signature(publicKey, randomDigits <> reference))
```

### What's in a Token?

In this formulation, Token is a possibly-parameterized catch-all that includes whatever information is necessary to securely authorize some use.

Stateless tokens will include:

  - A description of the authorized resource/activity, sufficient to be understood by the resource servers.
  - A signature by entity trusted by the resource server.
      - If the token is composite, each separable piece must be individually signed.  Signatures are typically the size of the key (4096 bits = 512 bytes), so they can start to add up.

They will optionally include:

  - An expiration / validity period - or be valid in perpetuity
  - An "audience", identity of the target resource server, in cases where the signature key is too broad to identify the resource server.

Example:

``` 
Token =
  abilities e_1, ..., e_n <> expiration
    <> signature ku ([e_1 ... e_n] <> expiration)
    <> fingerprint ku

or:
  (e_1 <> expiration <> signature ku (e_1 <> expiration) <> fingerprint ku)
<> ...
<>(e_n <> expiration <> signature ku (e_n <> expiration) <> fingerprint ku)
```

This is leading up to an exponential number of signatures, just to support `Loc.restrict`.  So, let's look at some schemes for delegation.

### Elastically producing new Locations

An elastic compute service “front-end” would expose:
1\. a function to `provision` new locations
2\. a Location at which the function could be run

  - Can I have this `provision` function in my namespace, without having its implementation in my codebase?

<!-- end list -->

  - \[ \] The implementation of `provision` would need some way to authenticate and validate the request.
  - \[ \] It would need some way to construct a Unison `Loc` value (not yet discussed).
  - \[ \] It should provide a way for the front-end to monitor utilization and spin up or shut down physical resources as needed.

*Idea*: Maybe the `Token` value provided by the front-end is structured in a provider-specific way, with whatever data is needed to make these decisions.  Having a distinct `Token` type for distinct providers means another type parameter on the `Loc`, which could answer the question about consolidating `Loc`s on the user side.  If two Locations share the same provider type, they can be consolidated (hosts, quotas, abilities); otherwise they obviously couldn’t be.

``` haskell
Remote.forkAt : Loc {e} p -> '({e} a) ->{Remote} Future a
Location.join : Loc {e} p -> Loc {e2} p -> Loc {e,e2} p
```

## Futures

A `Future` represents an asynchronous computation. `Remote.forkAt` takes a computation and returns immediately with a `Future`.  To wait for the computation’s output, use `Future.force`.

``` haskell
Remote.forkAt : Loc {e} ->'({e} a) ->{Remote} Future a
Future.force  : Future a ->{Remote} (Either Err a)
type Err = TBD

-- example:
f1 = forkAt a 'let
  x = longRunningComputation 101
  makeHistogram x
y = otherLongComputation
x = Future.force f1
Database.save (x, y)
```

  - How many times can a future be successfully forced? Suppose a future is shared with 5,000 machines. The task backing the future eventually completes, and now what?
      - The thought: the machines sending keepalives (subscribers?) are retained at the Location performing the computation; when the computation is complete, the Location should send the result back to those subscribers.  The subscribers save the result in their caches until they no longer reference the `Future`.
      - Random thing - if 5,000 nodes have a reference to a future, the status update / keepalive protocol should come with a response like "send me another keeplive within X time", where X is influenced by the number of other subscribers / density of keepalives. This prevents flooding the network with keepalives.

### Supervision and garbage-collection of Futures

Unison Futures can be monitored or terminated using:

``` haskell
Future.status : Future a ->{Remote} Future.Status
type Future.Status
  = Running LastUpdate | Canceled | Finished
  | Unreachable | Unresponsive

Future.cancel : Future a ->{Remote} (Either Err2 ())
type Err2 = TBD
```

To the extent that an async computation should be canceled if there is no other computation interested in its result, we need some way of determining whether or not this is the case.  We discussed having a system of keep-alives, absent which a Future might be canceled by its host:

``` haskell
-- these likely will just be handled by the interpreter
-- of Remote, not by "user" code.
Future.keepalive : Duration -> Future a ->{Remote} Status
Future.remaining : Future a ->{Remote} Duration
```

Moreover, there will be cases where we want to transfer or delegate the keep-alive responsibility for a long-running tasks to a more available location.

``` haskell
Remote.supervise : Loc {e} -> Future a -> {Remote} ()
Remote.unsupervise : Loc {e} -> Future a -> {Remote} ()
```

> We discussed producing a `Heartbeat` identifier along with any `Future`, but decided there was no benefit to separating the two.

We haven’t discussed how to prevent a delegate supervisor from accumulating and perpetuating many long-running Futures that will never actually be forced. With this in mind, have we gained anything from a GC perspective?

## Stationary data

We will need some notion of data that doesn't just move automatically with the computation, even if the computation references it.  We identified two reasons you might want to do this:

  - The data is big, and you don't want to copy it around willy-nilly.
  - The data is secret, and you don't want to accidentally ship it to another location, you want to be very explicit about when this happens (for instance, secret keys, etc).

More generally, we want a way of being explicit about when certain data is moved between locations, rather than implicitly relocating anything in lexical scope (this could be an API thing, a type-system thing, a code-analysis tool).

-----

## Notes/Desiderata

  - \[ \] Elastic computation - need to be able to talk about spawning new computing resources, and ideally this compute can be garbage collected as soon as you're done using it.
  - \[x\] `fork` a task to run on a separate thread or at another "location"
  - \[x\] Different locations may have access to different abilities (just pure computation, `IO`, `GPU`, etc)
  - \[x\] Need to be able to respond to location failures, with maximal flexibility. Allow different ways of doing failure detection/recovery.

<!-- end list -->

  - Locations are first-class, permissions, tasks, are first-class
      - \[x\] locations
      - \[ \] permissions?
      - \[x\] tasks (futures)

<!-- end list -->

  - \[ \] Some notion of data that doesn't just move automatically with the computation, even if the computation references it.
    
      - e.g., The data is big, and you don't want to copy it around willy nilly.
      - e.g., The data is secret, and you don't want to accidentally ship it to another location, you want to be very explicit about when this happens (for instance, secret keys, etc).
      - Might more generally want a way of being explicit about when data is moved to a location rather than just implicitly relocating anything in lexical scope (could be an API thing, a type system thing, a tool).

  - \[x\] Need to be able to launch a long-running computation and have it outlive the task / location / node that launches it. But then how do you interact with this computation later? (Say, to cancel it? Or to check if it's finished? Or more generally, how do you monitor it?)

  - \[ \] Need to be able to hash and serialize any Unison value, so that storage API(s) can be implemented in pure Unison.
    
      - Should the hash of a value know the type of the value? (`hash : a -> Hash a`)

  - \[x\] How do you represent `Loc{e}` to be securely verified by the receiving node? The `Loc{e}` must be unguessable and tamper-proof.
    
      - This is achieved by making the component `Token`s unguessable and tamper-proof.

  - \[ \] Must be safe to say `at loc1 loc2` without allowing nefarious loc1 to abuse loc2.  (Needs clarification.)

  - \[x\] The runtime needs an unguessable way (crypto?) to represent Locations and their abilities.

  - \[ \] Not all computations should have access to all data.
    
      - file system
      - individual durables

  - \[x\] Not all Locations should provide unlimited resources to all users (arbitrary computation, time, storage, bandwidth, priority).

  - \[ \] Not all data should be portable to arbitrary locations (think secret keys, top secret clearance, hipaa).

*Misc?*:

  - Mutable typed (durable if needed) state at each location
      - For v1, could not have this, just focus on batch computation
  - Dealing with weird networks? (nat-busting)
      - Maybe in implementation, but not explicit in v1 API
  - Well-defined semantics not just a bunch of implementation-defined gobbledygook
  - Do we need globally-addressed mutable state? e.g. node `a` can refer to mutable data on node `b`; or node `c` can mutate data on node `d`. Yes, probably.

## Choices

  - We decided that automatically cancelling a child computation when its parent terminates or delaying termination of of the parent until its children complete would break associativity in terms of parallelism when chaining computations, therefore `forkAt` doesn’t enforce any such conditions.  See more about cancellation & termination below, in “Supervision and garbage-collection of Futures”

\#unison

This document describes a set of core Unison functions for expressing fault-tolerant multi-node systems, including systems that must be dynamically updated and redeployed without downtime.

Remarks:

  - There's a mixture of old and new stuff here, see the [history section](#history) for background.
  - Some version of these APIs will be implemented on the new Unison runtime. We are looking to gather design feedback and possibly iterate the design before starting on the implementation.
  - Some of these primitives are rather low-level and imperative; it's expected that people will use them to build nicer APIs in pure Unison.
  - Questions are good if you have them; ask away in the comments and we'll curate that into a Q\&A section of the document.

Lastly, thank you to all who have contributed to this design or worked on earlier iterations\!

### The API

Unison computations can hop between nodes, can fail, can be forked to execute asynchronously, and can be supervised:

``` Haskell
-- Promote a pure value to `Remote`
Remote.pure : ∀ a . a -> Remote a

-- Sequencing of remote computations
Remote.bind : ∀ a b . (a -> Remote b) -> Remote a -> Remote b

-- The current node where the computation is executing
Remote.here : Remote Node

-- Transfer control of remainder of computation to target node
Remote.transfer : Node -> Remote Unit

-- Explicitly fail a computation for the provided reason
Remote.fail : ∀ a . Text -> Remote a

-- Sleep the current computation for the given duration
Remote.sleep : Duration -> Remote Unit

-- Start running a remote computation asynchronously, returning
-- a `Task` value that can be used for supervision
Remote.fork : ∀ a . Remote a -> Remote Task

-- Halt a running task (and any running subtasks) using the provided `Cause`
Task.stop : Cause -> Task -> Remote Unit

-- Obtain the `Cause` that caused a running task to complete
Task.supervise : Task -> Remote (Remote Cause)

-- Create a duration from a number of seconds
Duration.seconds : Number -> Duration

-- this is TBD
type Cause = Error Text Node | Completed | Cancelled | Unresponsive Node
```

Unison computations can provision new nodes:

``` Haskell
-- Like `Remote.spawn`, but create the node inside a fresh sandbox
Remote.spawn-sandboxed : Sandbox -> Remote Node

-- Like `Remote.spawn-sandboxed`, but use the provided symmetric key
-- to communicate with the returned `Node`
Remote.spawn-sandboxed' : Key -> Sandbox -> Remote Node

-- Create a new node 'in the same location' as the current node, sharing
-- current sandbox resources
Remote.spawn : Remote Node

-- Like `Remote.spawn`, but use the provided symmetric key
-- to communicate with the returned `Node`.
Remote.spawn' : Key -> Remote Node

-- Statically provision a `personal-info : Node`
node personal-info -- layout block starts here
  Sandbox 5% 10MB 3GB accept-from

-- TBD
type Sandbox =
  Sandbox CPU% Memory Storage (∀ a . Node -> Remote a -> Remote a)
```

We can encrypt / decrypt any value at all:

``` Haskell
-- Encrypt a value, requires `Remote` since we use random IV / nonce
encrypt : ∀ a . Key -> a -> Remote (Encrypted a)

-- Decrypt a value, or return `None` if key is incorrect
decrypt : ∀ a . Key -> Encrypted a -> Either DecryptionFailure a

-- `Key` is just a symmetric encryption key. We might generate keys via:

AES256.key : Remote Key
Blowfish.key : Remote Key
-- etc

-- TBD
type DecryptionFailure = WrongKey | AlgorithmMismatch | IntegrityFailure
```

Unison programs have access to mutable variables, which also serve as a concurrency primitive:

``` Haskell
-- Create an ephemeral `Box` on the current node; just a (GUID, Node) at runtime
Box.empty : ∀ a . Remote (Box a)

-- Put a value into the box, or if the box is full,
-- wait until a `Box.take` empties the box.
Box.put : ∀ a . a -> Box a -> Remote Unit

-- Remove and return the value in the box, or if the box is empty,
-- wait until a `Box.put` fills the box.
Box.take : ∀ a . Box a -> Remote a

-- Like `Box.take`, but leaves the value inside the box
Box.read : ∀ a . Box a -> Remote a

-- Read the current value inside the box or return `None` immediately.
-- Also returns a setter which returns `True` if the set was successful.
-- The `set` is successful only if the value inside the box has not
-- otherwise changed since the read, so this can be used to implement
-- "optimistic" atomic modifies.
Box.access : ∀ a . Box a -> Remote (Optional a, a -> Remote Bool)
```

Unison can resolve references dynamically on a node:

``` Haskell
-- Create a `Name`, which is a typed reference to a node-local value.
Name.make : ∀ a . Remote (Name a)

-- Lookup the node-local value associated with the `Name`.
Name.resolve : ∀ a . Name a -> Remote (Box a)

-- Declare `bob : Name Number` statically. The value bound to
-- the `Name` does not survive node restarting.
ephemeral name bob : Number

-- Declare `cluster-peers : Name (Vector Node)` statically. The current
-- value of `cluster-peers` survives node restarting.
durable name cluster-peers : Vector Node
```

Unison can make any value durable. `Durable` values are immutable:

``` Haskell
-- Move any value from RAM to local durable storage
Durable.store : ∀ a . a -> Remote (Durable a)

-- Synchronize any value AND ALL TRANSITIVE DEPENDENCIES
-- to local durable storage, returning `True` if the given `Node`
-- has that `Durable a` locally and the sync was successful.
Durable.sync-from : ∀ a . Node -> Durable a -> Remote Boolean

-- Load a durable value into RAM, assuming it exists on the given node
Durable.load-from : ∀ a . Node -> Durable a -> Remote (Optional a)

-- Returns a list of nodes that the Unison runtime believes could
-- successfully `Durable.load-from` or `Durable.sync-from` for the
-- given `Durable`.
Durable.peers : ∀ a . Durable a -> Remote (Vector Node)
```

Lastly, we can declare foreign functions:

``` Haskell
-- Declare `my-fn : Foreign (Number -> Remote Number)` statically
-- Bindings for some of these foreign declarations would be done
-- in some implementation-dependent way on Unison node container startup.
foreign my-fn : Number -> Remote Number

-- Ask the current node if it has a binding for a `Foreign a`
Foreign.ask : forall a . Foreign a -> Remote (Optional a)
```

## Notes on semantics and implementation details

A basic design principle: the Unison runtime should never contact another Unison node unless the user's program explicitly indicates that node should be contacted. Thus, the runtime cannot run any sort of background task that contacts other nodes (like upkeep for a DHT), nor can it implicitly choose which nodes to contact (like doing some sort of autodiscovery to find "good" peers). The idea here is to make the runtime "as dumb as possible", and move all intelligence to regular Unison libraries.

The `Task` returned by `Remote.fork` controls the entirety of the computation forked, including any subtasks forked. Stopping that `Task` stops anything that may be running underneath this fork.

Implementation notes on `Task.supervise`:

  - At runtime, a `Task` value contains a `Node` reference where the `Task` was originally forked.
  - To implement `Task.supervise`, the runtime maintains at each node a `Map Task (Timestamp, Status, Optional Node)`, tracking for each task a timestamped last update for that task (when it was running on the current node), and an `Optional Node` if the computation was transferred elsewhere. This `Map` can be pruned using some ad hoc policy (like retain 30s of data or 5000 entries). `Task.supervise` then just chases the computation, following these transfer links until it obtains a "recent enough" status update for the computation. If a node is unresponsive or unreachable, this eventually leads to an `Unresponsive` error being passed to the supervisor.

On node local storage:

  - The association between a `Name` and a `Box` is *local to the node*. Conceptually, each node has its own durable and ephemeral storage. There is no storage concept exposed by Unison at any granularity beyond nodes (though of course you can write multi-node storage as regular Unison libraries). Nodes are isolated from each other and must communicate explicitly (even if the nodes are all spawned in a single sandbox).
  - The `durable name blah : Name Number` is somewhat analogous to a typed file name. It can be resolved on any node to a `Box Number`, and the state of that `Box Number` (whether it is empty or full) will survive node restarts.
  - The `node node-name` block declares a node statically, by proving a `Sandbox`.
  - The various `Durable` functions give some flexibility to Unison programs in how they resolve `Durable` values and where they load them from.

On storage and discovery of `Durable` values:

  - It's expected that `Durable.load : Durable a -> Remote a` could be implemented in terms of `Remote.load-from` and `Durable.peers` (with a small chance of failure if all nodes delete durable data stored elsewhere).
  - A sketch of how `Durable.peers` map gets updated:
      - Any call to `Durable.load-from n1 d` for a `d` not already present on the current node gets an entry in the peers map.
      - When receiving a continuation via `Remote.transfer`, entries are added to the peers map for any durables not present on the receiving node. So if the continuation references `d : Durable Number`, and the sender's peer map for `d` was `[alice, bob, carol]`, then `[alice, bob, carol]` would be added to the recipient's peer map for `d`. If the sender's peer map is empty (because the sender has the `Durable` locally), we'd just add the sender to the peer map.
      - Successful calls to `Durable.sync-from` clear out peers map entries for that `Durable` and its transitive dependencies, since once it's stored locally, we stop caring where else we could get it from.
      - May want to prune the number of peers stored for a given `Durable`, if lots of peers have it.

### <a id="history"></a>Appendix: History and context

**Most recently (after discussion in [\#142](https://github.com/unisonweb/unison/issues/142)):**

  - Split `Capability` into `Foreign` (for the foreign function interface) and `Name`, for locally bound names.
  - Loading of `Durable` values is more explicit about *where* the values are being loaded from, but runtime provides enough info to implement good heuristics for discovering `Durable` values from peers more implicitly.
  - There's now a way to statically declare a `Node`, which is important for bootstrapping a system.

**V2 (after discussion in [\#141](https://github.com/unisonweb/unison/issues/141)):**

  - Got rid of `Clock` and `Index` in favor of immutable durable storage concept + mutable pointers.
  - Got rid of `Channel` in favor of `Box`, also simplified `Capability` API to just build on `Box` directly.
  - Got rid of `Heartbeat` arguments to a whole bunch of functions (like `Box.take`, etc), opting for just using the ambient lexically-scoped heartbeat established via `Remote.link`. 99% of the time this is what you want, and you can always push another `Heartbeat` onto the stack via a nested `Remote.link`.
  - Clarified behavior around lifetimes of `Remote.fork`-ed computations and `Remote.spawn*` nodes--they always inherit the current ambient heartbeat. I believe this is key for composability, since it makes the interface for shutting down a subcomputation completely uniform.

**Previously:**

[This post](http://unisonweb.org/2015-06-02/distributed-evaluation.html) has an early writeup of how Unison's hashing scheme could be used to build a robust multi-node computation story. That eventually got an implementation, and as a demo I put together [a simple multi-node search engine](http://unisonweb.org/2016-10-12/search.html#post-start) in Unison. That raised a couple issues and questions, some discussed in that post, some discussed [in this post about microservices](http://unisonweb.org/2016-10-12/microservices.html#post-start), and some that I have just been ruminating on. 🤔

The big questions were around:

  - Lifecycle management of nodes and durable data---when is durable data destroyed, and when are nodes destroyed? This led to the `Heartbeat` design (which was later scrapped).
  - Encryption: how are things encrypted, both at rest (in durable storage) and in transit (when moving between nodes). The solution given here is to have 'in transit' encryption handled transparently by the runtime, but to have encryption keys for durable state to be managed explicitly by the programmer. This allows for multiple nodes to use a common storage layer, without all reads needing to go through a common node.
  - Dynamic updates and redeployment---how is this done? Solution given is the `Capability` stuff.

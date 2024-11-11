## Distributed Garbage Collection

We use a weak `B` map, to track local boxes (entries are removed by virtue of being a weak map once they are no longer referenced in local heap / boxes):

``` haskell
B_map :: WeakMap BoxId (MVar Value)
```

and a weak `C` set, tracking all remote boxes referenced by local heap / boxes:

``` haskell
type RemoteBox = (BoxId, Node)
C_set :: WeakMap RemoteBox
```

Each local box `b` has an associated value, and associated set of boxes referenced by its contents, `b_subs`.

``` haskell
let keepaliveDuration = 20.seconds -- or whatever
type Keepalive = Keepalive { b :: BoxId, visited :: Set RemoteBox }
```

**Receiving Keepalives**
When node `n` receives a keepalive message for BoxId `b`

1.  If `n` doesn't own `b`, disregard (shouldn't occur)
2.  Else if `(b,n)` ∈ `visited`, disregard (normal occurrence)
3.  Else
    1.  Create a strong reference to `b` for a fixed period of time (`keepaliveDuration`)
    2.  Let `b_subs` be the set of all boxes (local and remote) referenced by `b`.
        1.  If `b_subs` is not cached, and no existing process is indexing `b`, starting indexing `b` and cache the result when complete.
        2.  If indexing does not complete in time, do not interrupt indexing, but use `C_set` as an approximation of `b_subs` for the purposes of processing this particular keepalive message.
    3.  For each `b_i` ∈ `b_subs`,
        1.  If `b_i` is a remote box, send `(Keepalive b_i (Set.insert (b,n) visited))` to the owner of `b_i`.
        2.  If `b_i` is a local box, process `(Keepalive b_i (Set.insert (b,n) visited))` locally.  Whether or not you hit the network is up to you, but in this scheme, we do need to recursively propagate keepalives through local boxes.

To compute `b_subs` (set of boxes referenced by the value inside the `b` box):

1.  Keep mutable cache `Optional [BoxId]` for each runtime value, `v`, tracking boxes referenced transitively by `v`.
2.  Do a deep scan of the `v` inside the box to fully populate caches, recursively.
3.  Avoid revisiting subtrees that already have a computed cache.

**Receiving Continuations or Box Updates**
When a continuation `c` is transferred from node `x` to node `y`, or when value `c` is `Box.put` from node `x` to node `y`, node `y` adds non-local boxes referenced by `c` to `C_set`.  (This indexing may be done as part of the network deserialization.)

We must ensure that boxes referenced by `c` are not GCed before `y` can issue keepalives; this means that node `x` must send keep-alives to any boxes referenced by `c` during the transfer (this should already happen without special care) and at least once more after the transfer has completed, to avoid a race condition while `y` takes over the keepalives.  This may mean that both nodes `x` and `y` must also index `c` while it is being transferred.

**FAQ**
Q: Will `C_set` contain all of the remote boxes referenced by local boxes?
A: Yes: to store a value into `b`, the value must be constructed within some continuation. Remote box references can only exist in a continuation transferred from a remote node, or a value `Box.put` from a remote node.  In both of these cases, any remote boxes referenced in the transfer are indexed into `C`, per "Receiving Continuations or Box Updates" above.

Q: Can we say that durable values don't keep boxes alive?  That a durable shouldn't expect any particular value to be preserved in a referenced box?
A: ...

Q: If a remote node has computed the `Optional [BoxId]` for a runtime value, should the remote node transfer that cache to me?
A: ...

**Optimizations**

  - Avoid allocating boxes to B-map and C-set until first transfer.  Until first transfer, boxes are just a regular `MVar` on the stack.

\*\* Example reference graph\*\*

``` haskell
type Foo = Ref (Box Foo) | No_Ref

do Remote
  Remote.transfer x
  q := Box.make
  r := Box.make
  Remote.transfer y
  s := Box.make
  t := Box.make
  Remote.fork <| do Remote
    sleep-random-duration
    Box.take t
  Box.put q (Ref s)
  Box.put s (Ref r)
  Box.put r (Ref t)
  Box.put t (Ref q)
  Box.put t No_Ref -- maintains cycle until Box.take t, then breaks cycle
```

``` text
    x     y
   ┌─┐   ┌─┐
 ┌>│q│──>│s│
 │ ├─┤  /├─┤
 │ │ │ / │ │
 │ ├─┤└  ├─┤
 │ │r│──>│t│
 │ └─┘   └─┘
 └────────┘
```

### Remembering local/remote codetree associations?

``` haskell
-- Designates remote paths to sync local paths against
newtype RemoteMap = RemoteMap { toMap :: Map (Path, RemoteName) RemotePath }
```

If I have some branch (tree node) that I want to sync with github on an ongoing basis.  e.g. `/projects/foo` to `github:aryairani/foo` — that becomes a place I can publish to or pull from, how should I associate the two?   If I

If I associate it by path, then what should happen when I move or copy the node in the tree?  What do I have to update to make that happen?

What happens if I associate it by `Causal` hash?

``` 
# parenthesized hashes represent the branch hash

/projects (mZm)> remote.set github:user/foo foo
	Set remote github:user/foo for /projects/foo (0e9).
```

/projects/foo (0e9) linked to github:user/foo

``` 
/projects (mZm)> cp foo foo-fork
/projects (wkP)> cd foo-fork
/projects/foo-fork (0e9)> add myFunc 
  Added myFunc.
/projects/foo-fork (p3z)> 

Should now have:
/projects/foo (0e9) linked to github:user/foo
/projects/foo-fork (p3z) linked to github:user/foo
```

``` 
# types
.unison/types/<hash>/compiled.ub
.unison/types/<hash>/dependents/<hash>
.unison/types/_builtin/<base58>/dependents/<hash>
# terms
.unison/terms/_builtin/<base58>/dependents/<hash>
.unison/terms/<hash>/compiled.ub
.unison/terms/<hash>/type.ub
.unison/terms/<hash>/dependents/<hash>
# branches
.unison/branches/<hash>.ubf
.unison/branches/head/<hash> -- if several, merge entries to produce new head.
# edits
.unison/edits/<guid>/<hash>
.unison/edits/<guid>/name/<base58> -- (base58encode (utf8encode "name of the edit"))
.unison/edits/<guid>/head/<hash> -- if several, merge entries
# remotes
.unison/remotes/
```

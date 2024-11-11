-----

### Dependents

The reason we keep track of dependents is for the `todo` calculation.  When we make an edit, what are the things that need to be updated as a result?

When adding term `a` that depends on "derived" term `b` or type `B`, then a change to `b` or `B` affects `a`, so we record that `a` is a dependent of `b` and `B`.

When adding type `A` that depends on type `B`, a change to `B` affects `A`, so we record that `A` is a dependent of `B`.

We don't do anything for constructors, because constructors don't change.  Depending on the constructor really means you depend on the type that constructor comes from. (i.e. a constructor doesn't have dependents.)  Similarly, constructor doesn't have dependencies, but its declaring type may depend on other types.

-----

Commands

``` 
/> cd libs/Foo
/libs/Foo> cd ..
/libs> fork Foo Foo2
/libs> fork <someurl> thing
/libs> fork Foo /outside/Foo
/libs> fork /outside/Foo /outside/Foo2
/libs> help merge
					`> merge src dest`
/libs> merge /outside/Foo Foo
/libs> merge Foo2 Foo

/libs/Foo> <work work work>
/libs> move /libs/Foo /libs/Foo'
/libs> 

A.B.c
A.B.d

arya renames, and has: ->
A.Z.c
A.Z.d

paul adds, and has ->
A.B.e
A.B.c
A.B.d

then merge -> 
"Merge introduces the following aliases:"
A.Z.c -> A.B.c
A.Z.d -> A.B.d

/libs> delete /libs/Foo
"warning: /libs/Foo includes the following definitions that aren't anywhere else:

	A.B.e#123

run it again to proceed with deletion"

/libs> alias /libs/Foo/sqrt /libs/Foo2/butt
-- we talked about combining alias & fork into a single "copy" command
/libs> 
```

Weird thing: There's no history for `sqrt`\!

Suppose:

``` 
data Raw = Raw
  { _termsR :: Set Referent
  , _typesR :: Set Reference
  , _childrenR :: Map NameSegment Hash
  }
```

``` 
/libfoo/Foo <- type
/libfoo/Foo <- constructor
/libfoo/Foo.f <- term in child namespace

/libfoo> move Foo Foo2
/libfoo> alias Foo Foo2
```

``` 

```

## Data types:

Old **<u>PrettyPrintEnv</u>** is for pretty-pretting code, and \_\_\_

``` haskell
{ terms :: Referent -> Maybe HashQualified
, types :: Reference -> Maybe HashQualified }
```

Q: How do we want to handle lookup of names that are outside of our branch?

Old **<u>Namespace</u>**

``` haskell
{ _terms :: Relation Name Referent
, _types :: Relation Name Reference }
```

Old **<u>Names</u>** is an unconflicted **<u>Namespace</u>**. is for parsing code?  Not sufficient to parse hash-qualified names.

``` haskell
{ termNames :: Map Name Referent
, typeNames :: Map Name Reference }
```

New **<u>Names</u>** combines old **<u>PrettyPrintEnv</u>** and old **<u>Names</u>**:

``` haskell
-- these HashQualified are fully qualified
{ terms :: Relation HashQualified Referent
, types :: Relation HashQualified Reference }
```

We should be able to construct one from a `Codebase2`, given:

``` haskell
root :: Branch
current :: Branch
terms :: Set HashQualified
types :: Set HashQualified
```

or

``` haskell
root :: Branch
current :: Branch
terms :: Set Referent
types :: Set Reference
```

### Needed functionality

Parsing a .u file:

  - Look up a Reference by name

  - Look up a Reference by hash-qualified name?  We could avoid this by requiring that the user deconflict the names before parsing.

Parsing command-line arguments:

  - Look up a Reference by name.

  - Look up a Reference by hash-qualified name (possibly from among deleted names); for resolving conflicted names and edits.
    
    ``` 
    /foo> todo
    
    These names are conflicted:
      foo#abc
      foo#xyz
    Use `rename` to change a names, or `unname` to remove one.
    
    These edits are conflicted:
      bar#fff -> bar#ggg : Nat        (12 usages)
      bar#fff -> bar#hhh : Nat -> Nat (7 usages)
      bar#fff (Deprecated)
    
    Use `view bar#ggg bar#hhh` to view these choices.
    Use `edit.resolve` to choose a canonical replacement.
    Use `edit.unreplace` to cancel a replacement.
    Use `edit.undeprecate` to cancel a deprecation.
    Use `edit.replace bar#hhh bar#ggg` to start replacing the 7 usages of `bar#hhh` with `bar#ggg`.
    
    /foo> alias bar baz
    
    Not sure which bar you meant?
      bar#ggg
      bar#hhh
    Try specifying the hash-qualified name, or sort out the conflicts before
    making the alias.
    ```
    
    ``` 
    /foo> edit.resolve bar#fff bar#ggg
    
    Cleared bar#fff -> bar#hhh
    Added   bar#ggg -> bar#hhh
    ```
    
    or
    
    ``` 
    /foo> edit.unreplace bar#fff bar#ggg
    
    Cleared bar#fff -> bar#ggg
    ```

Pretty-printing:

  - Select a name by Reference

<u>Q: What to do about names outside the current branch?</u>

Option 1: Don't support names outside the current branch; user must go up a level (possibly to the root), set up the names as desired, and then descend again.

Option 2: Introduce some syntax for names outside the current branch, e.g. `_root_.Foo.bar`. We could first lookup references in the current branch, then in the root branch, then in the history of the root branch?

## TODO tracking refactoring of existing functionality

  - \[ \] Add edits/patches to Namespace / Branch

  - \[ \] Add patch to `NameTarget`

  - \[ \] rename `propagate` to `patch`
    
      - moves names from old hash to new hash, transitively, to the type-preserving frontier

  - \[ \] `list [path]`
    
      - ~~by default, don't descend into links with names that start with `_`~~

  - \[ \] `todo <edit> [path]`
    
      - list conflicted names (hash-qualified) and edit frontier

  - \[ \] `update <edit> [path]`
    
      - ~~when updating a term, old names goes into `./_archived`, which will be largely conflicted.~~

  - \[ \] `propagate <edit> [path]`

  - \[ \] `edit.resolve <patch> <hq> <hq>`

  - 
Old names use case 1:

``` 
patch:
#a -> #b
#a -> #c

namelookup:
#b -> "foo"
#c -> "foo2"

"You have a conflicted edit:
   #a -> foo#b
   #a -> foo2#c
 Please choose one.
"

/pc/libs/x> edit.resolve #a foo#b
```

You're in the middle of an edit, it's not type preserving

  - \[ \] `rename / move`
    
      - \[ \] `rename.edits`
      - \[ \] `rename.type`
      - \[ \] `rename.term`

  - \[ \] name / copy `copy <[src][#hash]> <newname>`

  - \[ \] `todo <edit> [path]`, `update <edit> [path]`, `propagate <edit> [path]`

  - \[x\] Implement `Branch.sync` operation that synchronizes a monadic `Branch` to disk

  - \[x\] Implement something like `Branch.fromDirectory : FilePath -> IO (Branch IO)` for getting a lazy proxy for a `Branch`
    
      - Also `Branch.fromExternal : (Path -> m ByteString) -> Hash -> m (Branch m)`
      - Could we create a `Branch` from a GitHub reference? Seems like yeah, it's just going to do some HTTP fetching.

  - \[x\] Tweak `Codebase` to `Codebase2`

  - \[x\] Implement a  `Codebase2` for `FileCodebase2`

  - \[ \] Implement `Actions2`

  - \[ \] Implement `Editor2`

  - \[ \] Implement `OutputMessages2`

  - \[ \] Implement `InputPatterns2`

  - \[ \] Go back and leave a spot for Link in serialized Branch0 format.

  - \[ \] Split Edits out of `Branch0`

  - \[ \] Delete `oldNamespace`, and instead add deprecated names

  - \[ \] Parsing takes a `Names`, a map from `Name`(fully-qualified name) to `Referent`/`Reference`.  We should switch these from `Map` to `Name -> Optional xxx`, or even `Name -> m (Optional xxx)`

  - \[ \] `Context.synthesizeClosed` takes a `TypeLookup`, which includes a map from `Reference` to `Type`, `DataDecl`, `EffectDecl`.  Shall we plan to include the full codebase here, or load them on demand?  Maybe it doesn't matter yet.
    
      - `parseAndSynthesizeFile` takes  a `Set Reference -> m (TypeLookup v Ann)`, maybe that's a good model.

  - \[ \] `add` and `update` will need a way to update the `Branch'` at the current level, and all the way back to the root.  Some kind of zipper?

  - \[ \] `find` takes an optional path

  - \[ \] `fork` takes a `RepoPath` (or we could have a dedicated command like `clone`)

  - \[ \] `merge` takes at least a path, if not a `RepoPath`

  - \[ \] `publish` or `push`that takes a local path and a remote path?

## Branchless codebase format

## Commands / Usage

``` 
/> clone gh:aryairani/libfoo
  Copied gh:aryairani/libfoo blah blah to /libfoo
/> undo
/> clone gh:aryairani/libfoo /libs/DeepLearning/Foo
  Copied gh:aryairani/libfoo blah blah to /libs/DeepLearning/Foo
/>
```

`clone <remote> [path]`

`push [path] <remote>`

``` 
/> cd projects
/projects> rename FaceDetector FaceDetector/V1
/projects> cd FaceDetector
/projects/FaceDetector> cp V1 V2
```

`cd <path>` — support relative paths?

`cp <path> <path>`

``` 
/projects/FaceDetector> replace.scoped V2 /libs/DeepLearning/Foo/thing1 mything1

  Noted replacement of thing1#af2 with mything#i9d within /projects/FaceDetector/V2.
```

``` 
replace.write <editsetid> <ref1> <ref2>
todo <editsetid> <path>

```

``` 
/projects/FaceDetector> todo
  ...7 things...
/projects/FaceDetector> todo /
  ...33 things...
/projects/FaceDetector>
```

`mv` / `rename` command: can refer to Terms, Types, Directories, or all three.  Use hash-qualified names to discriminate.

## Namespaces

``` haskell
data Branch' m = Branch' (Causal m Namespace)

data Causal m e
  = One { currentHash :: Hash, head :: e }
  | Cons { currentHash :: Hash, head :: e, tail :: m (Causal e) }
  -- The merge operation `<>` flattens and normalizes for order
  | Merge { currentHash :: Hash, head :: e, tails :: Map Hash (m (Causal e)) }

-- just one level of name, like Foo or Bar, but not Foo.Bar
newtype NameSegment = NameSegment { toText :: Text } -- no dots, no slashes
newtype Path = Path { toList :: [NameSegment] }

data Namespace m = Namespace
	{ terms :: Relation NameSegment Referent
  , types :: Relation NameSegment Reference
  , children :: Relation NameSegment (Branch' m)
  }
```

**<u>Repo format:</u>**

``` 
# types
.unison/types/<hash>/compiled.ub
.unison/types/<hash>/dependents/<hash>
.unison/types/_builtin/<base58>/dependents/<hash>

# terms
.unison/terms/<hash>/compiled.ub
.unison/terms/<hash>/type.ub
.unison/terms/<hash>/dependents/<hash>
.unison/terms/_builtin/<base58>/dependents/<hash>

# branches (hashes of Causal m Namespace)
.unison/branches/<hash>.ubf
.unison/branches/head/<hash> -- if several, merge to produce new head.
```

### Backup Names?

For pretty-printing, we want a name for every hash.  Even for hashes we deleted the names for. 😐

  - When we delete a name `x` from path `/p` (i.e. `/p/x`), we add the name `/_deleted/p/x`.  <!-- pchiusano: I like this option, it's simple -->

  - Or, do we just disallow removing the last name of things with dependencies?

  - When deleting a name, notify the user of the remaining names.

## Edits

``` haskell
newtype EditMap = EditMap { toMap :: Map GUID (Causal Edits) }

data Edits = Edits
	{ terms :: Relation Reference TermEdit
	, types :: Relation Reference TypeEdit
	}

type FriendlyEditNames = Relation Text GUID
```

**<u>Repo format:</u>**

``` 
.unison/edits/<guid>/<hash>
.unison/edits/<guid>/name/<base58> -- (base58encode (utf8encode "name of the edit"))
.unison/edits/<guid>/head/<hash> -- if several, merge to produce new head.
```

### TODO: How to share these edits?

  - It could be the same as sharing Unison names (e.g. if the edits were Unison terms)
  - It could be the same as sharing Unison definitions:
    Make up a URI that references a repo and an edit GUID.
    e.g. `https://github.com/<user>/<repo>/<...>/<guid>[/hash]`
  - `clone.edits <remote-url> [local-name]`
      - `guid` comes from remote-url, and is locally given the name `local-name`
      - if `local-name` is omitted, then copy name from `remote-url`.
      - if `local-name` already exists locally with a different `guid`, then abort.

### Editsets as first-class unison terms:

Benefits:

  - Don't have two separate dimensions of forking and causality (namespace vs edits).
  - Makes codebase model way simpler to explain. \<— BFD

Costs / todo:

Q: Do we allow users to edit `EditSets` using standard `view` and `edit` in M1?

If Yes:

  - EditSets are arbitrary Unison programs that need to be evaluated.  Once evaluated, they would have a known structure that can be decomposed for EditSet operations.   We would need:

  -   - \[ \] some new or existing syntax for constructing EditSet values
      - \[x\] a way to evaluate these unison programs
      - \[ \] a way to save evaluated results back to the codebase / namespace
          - Q: Do we evaluate and save these eagerly or lazily?
      - \[ \] a way in Haskell to deconstruct the EditSet value
      - \[ \] a way to modify (append to) values of that type using CLI commands.  e.g. `update` ?
          - either `update` calls a unison function that

If no (we don't provide user syntax for constructing `EditSets` in .u file):

  - EditSets are part of the term language?
  - Or a constructor with a particular hash? (Applied to Unison terms)

## Collecting external dependencies

If a subtree references external dependencies, they should be given local names when exporting.

Given:

``` 
/A/B/c#xxx
/D/E/f#yyy (depends on #xxx, #zzz)
/D/G/h#zzz
/libs/G/bar#zzz
```

If `/D/E` is published, what names should be assigned to `#xxx`, `#zzz`?

### Idea 1: Names relative to nearest parent

Collect external dependencies under `Dependencies`, using names relative to the nearest parent in common with the publication point?

i.e.:

``` 
f#yyy
Dependencies/A/B/C#xxx
Dependencies/G/h#zzz
```

<!-- pchiusano:
I like this option the best. Reasons:
- Option 2 seems ill defined and probably complicated, so let's nix that.
- Option 3 is simple, but is more work for the user, and also the easiest way for the user to address is to copy the whole tree of whatever dependent library they are using, even if they are just using a handful of functions. An automated procedure can produce a minimal set of named dependencies.
- Having a somewhat opinionated convention like this makes the code easier to read - you can easily view the minimal third-party dependencies used by a library.
- Option 1 doesn't preclude you from picking some other convention for where you put those dependencies, if you really want.
-->

<!-- atacratic:
Under 'collecting external dependencies', I guess a difficult case for idea 1 'names relative to the nearest parent' would be the following:

/A/B/c#xxx
/D/E/f#yyy (depends on #xxx, #zzz)
/D/A/B/c#zzz
since it would want to call both the dependencies Dependencies/A/B/c.

Maybe the prefix which you decide to be the effective root, has to be a single choice across all the dependencies of all the names being exported, rather than chosen on a per-dependency basis.

But anyway seems like there will need to be way to override these choices of dependency names - stitching together a namespace from several sources can't be done in a way that is both natural and automatic.

On idea 3 I think there has to be a default proposal - you can't force people to go choosing N new names.

(On backup names, I think the /_deleted/p/x idea is a bit painful because people will want to completely delete names, without leaving a trace. Also it looks a bit janky. Better to just say you can't delete the last name of something that is referenced.)

====

Is all manipulation of namespaces going to happen interactively at the CLI? ('import this namespace from here to there with this prefix'.) Is that maybe bad for the same reason that repls are bad (and watch statements are awesome)? I guess the alternative would be having a little language for stitching together namespaces.
-->

### Idea 2: Somehow derive from qualified imports used?

If

### Idea 3: Surface the condition\* to the user

\*the condition = the publication node contains definitions that reference definitions not under the publication node.

Ask them to create aliases below the publication point?

### Idea 4: Add external names to `./_auxNames/`

The nearest aux-name would only be used to render code only if there were no primary names known.

### Idea 5: Something with symlinks

``` haskell
data Branch' m = Branch' (Causal m Namespace)

data Causal m e
  = One { currentHash :: Hash, head :: e }
  | Cons { currentHash :: Hash, head :: e, tail :: m (Causal m e) }
  -- The merge operation `<>` flattens and normalizes for order
  | Merge { currentHash :: Hash, head :: e, tails :: Map Hash (m (Causal m e)) }

-- just one level of name, like Foo or Bar, but not Foo.Bar
newtype NameSegment = NameSegment { toText :: Text } -- no dots, no slashes
newtype Path = Path { toList :: [NameSegment] }

data Namespace m = Namespace
	{ terms :: Relation NameSegment Referent
  , types :: Relation NameSegment Reference
  , children :: Relation NameSegment (Link m)
  }

data Link m = LocalLink (Branch' m) | RemoteLink RemotePath
data RemotePath = Github { username :: Text, repo :: Text, commit :: Text } -- | ... future
```

This lets us avoid redistributing libs unnecessarily — let the requesting user get it from wherever we got it from.  But it doesn't specifically address this external naming question.

We might be publishing `/app/foo` which references definitions we got from `repo1`.  Somewhere in our tree (possibly under `/app/foo` and possibly not?) we have a link to `repo1`.

Somewhere under `/app/foo` we reference some defn from `repo1`.

Transitive publication algorithm:

  - find all the things that you're referencing
  - the things you're publishing that aren't under the pbulication point need to be resolved
      - they're local, and need to be given names under the publication point
          - user is notified, or we do something automatic
      - they're remote, and we need to include, in the publication, a link to the remote repo.
          - user is notified, or we do something automatic
  - "Something automatic" will be:
      - mirror the dependency names from our namespace into `./_Libs`; if it would produce naming conflicts to use `./_Libs`, then `_Libs1`, etc.
      - Or, just dump them into `./_Libs` and if doing so produces naming conflicts, force the user to resolve them before publishing.

## Syncing with remote codetrees

``` haskell
-- names tbd
data BranchPath = BranchPath RepoRef Path
data RepoRef = Local | GithubRef { username :: Text, repo :: Text, treeish :: Text }

```

``` 
/libs/community/DL
```

becomes
​\`\`\`haskell
BranchPath Local (Path \["libs","community","DL"\])

``` 



```

gh:<user>/<repo>\[/<path>\]\[?ref=<treeish>\] -- defaults to repo's `default_branch`

e.g. gh:aryairani/unison/libs?ref=topic/370

```` 
becomes
​```haskell
BranchPath (GithubRef "aryairani" "unison" "topic/370") (Path ["libs"])
````

or

``` 
gh:user/repo[:treeish][/path]

e.g. github:aryairani/unison:topic/370/libs
```

becomes

``` haskell
BranchPath (GithubRef "'aryairani" "unison" "topic/370") (Path ["libs"])
```

## Github Notes

Github uses a few different URL schemes.  They call the ones you can pluck off their website "html\_url"s.  They let you refer to files and directories, and can be parameterized by git *treeish* (branch, tag, commit).

We can interpret these to refer to the root of a namespace. https://github.com/unisonweb/unison can be interpreted as:

``` haskell
GithubRef "unisonweb" "unison" <$> getDefaultBranch "unisonweb" "unison"
```

The Github website will let you navigate to a git branch, e.g https://github.com/unisonweb/unison/tree/topic/370/ can be interpreted as:

``` haskell
GithubRef "unisonweb" "unison" <$> matchBranch "unisonweb" "unison" "topic/370/"
```

Branch names can contain slashes, such as `topic/370`, complicating parsing if there's meant to be path info following the branch name.

1.  Fortunately, if you have a git branch `a/b` then it's not possible to create branches `a` or `a/b/c`.  So you can load the [list of branches](https://api.github.com/repos/unisonweb/unison/branches) from JSON, and then test them against that treeish-prefixed path without ambiguity.
2.  Github's website doesn't know how to navigate into `Causal` structures, so it's never going to give us URLs with paths into a Unison namespace.  So maybe this is a moot point.

So, I would still go ahead with the made-up `gh:username/repo[:treeish][/path]` URI scheme; we can try to support the other URLs mentioned above, and let them refer to the root of the published namespace.

Our Javascript viewer can be made to create URLs with query params or fragments in them that can indicate the Unison path, and those can be the ones we share in tweets, etc:

http(s)://<username>.github.io/<projectname>?branch=<hash>\&path=<path> with the default branch being the head, and the default path being `/`.

``` 


```

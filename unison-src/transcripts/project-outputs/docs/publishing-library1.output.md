# Using library code in my application

## Current status

We've thought of library code as coming from a different branch, which we incorporate by merging branches.  (There's no other way to utilize a branch, except to merge it.)

Branches come from the `.unison` directory on disk, and `.unison` directories from external sources can be merged externally by recursively merging the directories.  When two `.unison` directories each contain a branch with a given name, the two branches are merged by the `unison` tool.

Each branch consists roughly of a `(Name, Reference)` "namespace" relation, and an `(Reference, Replacement)` "edits" relation.

### Some problems with this

  - There are a lot of steps:
      - Download someone's repo
          - Make a new dir and git clone to it?
      - Figure out how to recursively merge directories
      - Maybe that's not that many steps.
  - We incorporate all of the incoming branch's names (including dependency names?), whether you want them or not.
      - This by itself isn't necessarily a dealbreaker, but it implies a lot of energy (or tooling) will be needed to provide immaculate namespaces in published libraries.
  - We incorporate and activate all of the incoming branch's edits, whether you want them or not.
      - Ditto

### Question about collaborative edit semantics

If you rename `foo` to `bar`, and I upgrade `foo#a` to `foo#b` and share my work with you, should you end up with `bar#a` and `foo#b`, or just `bar#b`?

## What might be nicer?

### A built-in way to get a published branch

#### Idea: Provide a command to create a local branch from a Github repo

``` 
app> branch.clone gh:<user>/<ghrepo>[:ghbranch][/<ubranch>] lib

  Got 17 definitions from gh:user/ghrepo:ghbranch/ubranch

lib>
```

#### Idea: Let existing branch commands support `gh:` scheme

``` xml
branchspec
  := 'gh:' <user> '/' <repo> [ ':' <treeish> ] [ '/' <ubranch> ]
   | <ubranch>
```

``` 
master> branch.checkout gh:aryairani/either

  Synced 23 definitions. -- example output, idk

gh:aryairani/either> branch.checkout meetup3

  Ok.

meetup3>
```

Question: Can the "current" branch be remote, or do we need to introduce remote-tracking branches like git does; the former seems simpler IMO.  We would need an offline mode for a branch, and it should be as transparent to the user as possible.

### Use a branch without first merging?

#### Idea: Reference other branches via qualified imports

``` 
prefix := wordyId [ '.' prefix ]
id := [ prefix '.' ] ( wordyId | '(' symbolyId ')'
ids := id [ ' ' ids ]

importspec
  := 'import ' branchspec [ '/' prefix ] [ '(' ids ')' ] [ ' hiding (' ids ')' ]
   | 'import ' branchspec [ '/' prefix ] [ ' as ' prefix ]
```

Sample program:

``` 
import experiment2 as e2 -- embed a local branch into the current namespace
import gh:aryairani/either as Either -- embed a git branch into the cur. namespace

foo = Either1.Either.rightToOptional (e2.runExperiment data)
```

##### Redundant qualifiers?

  - Adding another (qualified) prefix to identifiers in a branch without also removing some leads to unnecessary line noise: `Either.Either.rightToOptional`.
    
      - We could reference deeper into a branch for our qualified imports:
        
        ``` 
        import gh:aryairani/either/Either as Either
        foo = Either1.rightToOptional
        ```
        
        Now we've imported only names prefixed with '`Either.`' from `aryairani/either`, and can refer to them by prefixing them with '`Either.`', i.e. `Either.rightToOptional` instead of `Either.Either.rightToOptional` in the previous example.

#### Idea: Branch-qualified identifiers

We can add a syntax for branch-qualified identifiers, then proceed with normal branch-management commands, then proceed with normal branch-management commands.

``` 
meetup3> alias gh:aryairani/either/Either.rightToOptional Either.rightMay
  ┌
  │  ✅
  │  
  │  I aliased the term gh:aryairani/either/Either.rightToOptional to
  │    Either.rightMay.
  └

meetup3>
```

This is pretty first-order and terrible.

#### Idea: Merge libraries not at their roots

``` 
meetup3> merge gh:aryairani/either as Arya

  Copied 17 names.  Use `details` to list them.

meetup3> view Arya.

	Arya.Either.rightToOption : Either a b -> Option b
	Arya.Either.leftToOption : Either a b -> Option a
	...

meetup3>
```

#### Idea: `import` statements are 1st class entities

`import` statements could be first-class things that are added to the namespace on an `add`.

> Side note: This reminds me, I think there are reasons to reconsider adding support for `add`ing individual definitions from .u to branch.  I have a WIP for this, but it doesn't work.  😅  Could probably knock it out quickly by pairing.

Anyway, if we `>add` on this file,

``` haskell
import gh:ghuser/ghrepo:treeish/unisonbranch as Foo
import gh:arya/either:either//Either as E -- 🤔 so many "either"
bar x = E.fromJust (Foo.foo x) + 1
```

we also add an entry to the namespace:

``` haskell
("Foo", QualifiedImport (Github "ghuser" "ghrepo" (Just treeish) "unisonbranch") Nothing)
("E", QualifiedImport 
				(Github "aryairani" "either" Nothing "default?master?") 
				(Just "Either") )
```

where

``` haskell
data BranchSpec
  = Local UBranchName
  | Github Username Repo (Maybe Treeish) UBranchName

data QualifiedImport = QualifiedImport
  { branchSpec :: BranchSpec
  , from :: Maybe Prefix
  , as :: Prefix
  }
```

This could be a Haskell value or a Unison term.  `import` could also be a CLI command (syntax tbd).

We can copy any remote data  to a github cache under `.unison/cache/gh/gh-commit-id` or `.unison/cache/gh/ghuser/ghrepo/gh-commit-id` or whatever, and reuse it from there, or refresh it according to some protocol.

When I reference `E.fromJust` or `Foo.foo` it looks in the branches it downloaded from github.  The names of transitive dependents are added to "oldnames", so if the remote name goes away, or the link is deleted, we still have some text to display.  If `treeish` is a git hash, it would refer to an immutable thing, so it could be cached permanently.

#### Idea: First class namespace — move this to publishing section?

This is basically the previous idea but allowing for more complex structure.  Instead of just being a link to a remote namespace in its entirety, we could have a single value that describes many imports; these structures can be imported in the same way within .u files, Github gists, etc.

``` 
prefix := wordyId [ '.' prefix ]
id := [ prefix '.' ] ( wordyId | '(' symbolyId ')'
ids := id [ ' ' ids ]

importspec
  := 'import ' branchspec [ '/' prefix ] [ '(' ids ')' ] [ ' hiding (' ids ')' ]
   | 'import ' branchspec [ '/' prefix ] [ ' as ' prefix ]

namespace := 'namespace ' id ' where' [ imports, defs ]
```

Sample program:

``` haskell
namespace AryaPack where
  -- can reference local branch experiment1's `dataset` as `e1.dataset`
  import experiment1 as e1 -- embed a local branch into the AryaPack namespace
  -- Can reference runar's Multiset.Multiset.empty as Multiset.empty
  import gh:runarorama/Multiset (Multiset.fromList)
  -- Can reference paul's Simple.Example.Example1 as AryaPack.Example1
  import gh:pchiusano/EasyTest/Simple.Example as Example

  myFunc = Multiset.fromList (Example.summarize e1.dataset)
```

The above becomes a term named `AryaPack : Namespace`, which I somehow get into my github aryairani/AryaPack project.

  - Basically this is syntax sugar for defining a special Unison object.  We could also define it with normal Unison constructors, although it would probably be uglier.
  - The above program includes a definition along with the imports, but that doesn't have to be allowed.

Then the program below works:

``` haskell
import experiment2 as e2 -- embed a local branch into the current namespace
import gh:eed3si9n/hello as Hello -- embed a git branch into the cur. namespace
from gh:aryairani/AryaPack/AryaPack import myFunc
--   ^^ repo       ^^ branch ^^ term; in this case, a namespace
```

## 

#### Question: When do we actually download stuff?

When do we actually bring those names/definitions into the local codebase, so we can view dependents without being online, or if the import statements are removed from .u file?

##### Idea: Copy referenced names/defs into the branch

If we `>add` on this file:

``` 
import gh:aryairani/either/Either as Either
foo = Either1.rightToOptional
```

we get a temporary copy of the `gh:aryairani/either` branch (maybe greedily get the whole remote codebase, or maybe stream data as needed), use it to retrieve names and dependencies of any symbols we may try to resolve against it.  If `foo` is added to the local branch, then we save the names of those remote dependencies into the local branch as well.

###### Question: What names do we assign to unreferenced dependencies?

### What if the codebase were a tree, rather than a list of branches?

\#\#\#\#Hand-wavy example

``` 
/> clone gh:aryairani/libfoo
  Copied gh:aryairani/libfoo blah blah to /libfoo
/> undo
/> clone gh:aryairani/libfoo /libs/DeepLearning/Foo
  Copied gh:aryairani/libfoo blah blah to /libs/DeepLearning/Foo
/>
```

Sorry that I am using `/` and `.` interchangeably.

I'm using  `.`, because it's the typical code identifier separator we're used to, and I'm using `/` because it looks like directories and also commonly represents a tree root.  `.` doesn't feel good as a tree root, because it common represents the "current" node in a tree.  There's also the Scala route of `.` separator and `_root_` means the tree root. 😅

Anyway, we have some kind of structure like:

``` 
/Builtin
/libs/UJson
/libs/Stream
/libs/DeepLearning/Bar
/libs/DeepLearning/Foo
/projects/BoringCrudApp
/projects/ChordProgressions
/projects/FaceDetector
```

``` 
/> cd projects
/projects> rename FaceDetector FaceDetector/V1
/projects> cd FaceDetector
/projects/FaceDetector> cp V1 V2
-- <add mything1 to codebase>
/projects/FaceDetector> replace.scoped V2 /libs/DeepLearning/Foo/thing1 mything1

  Noted replacement of thing1#af2 with mything#i9d within /projects/FaceDetector/V2.

/projects/FaceDetector> todo
  ...7 things...
/projects/FaceDetector> todo /
  ...33 things...
/projects/FaceDetector>
```

#### How do you reference code in a system like this?

##### Idea: Absolute imports

.u:

``` haskell
import /projects/FaceDetector/V1 as V1
-- or: import _root_.FaceDetector.V1 as V1
compareResult = foo V1.result result
```

CLI:

``` 
projects/FaceDetector/v2>
  Typechecked the following definition:
  compareResult : Result
```

vs

``` 
projects/FaceDetector>
	Typechecked the following definition:
	compareResult : V2.Result
```

##### Idea: Relative imports

``` haskell
import ../V1 as V1
-- or: import _parent_.V1 as V1
```

##### Also: TDNR

Given:

``` 
/foo/bar/Bar.baz -- #abc
/blah/wah/Bar.baz -- #xyz
```

TDNR candidates are `foo.bar.Bar.baz` and `blah.wah.Bar.baz`

##### Benefit: Organize your shared repo to arbitrary depth

``` haskell
import gh:aryairani/awesome-unison/alltheparsers/specificparser/submodule as M
```

#### What are the units of code sharing and collaboration?

You can easily imagine exporting a subtree, but what if that subtree references definitions that are outside of it?  e.g. you want to share `/Foo/`, but `Foo.bar` references `/Quuz.quuzCount`?

  - Unison could warn you, and help you stage a subtree to publish. "I can collect all these referenced names into a subtree for you to bulk edit"

  - Unison could make up / choose some appropriate names based on the current tree:
    
    ``` haskell
    namespace Dependencies where
    	static import /libs/Foo as Abc -- this is replaced by a full/static copy of the names
    	static import /temp/Bar as Xyz -- some other library code in this subtree uses
    ```
    
    In this next syntax block, I'm tagging subtrees with a publication location, to avoid needing to have separate unison repos on your local machine for each project.  e.g. One repo would have all your preferred customizations.
    
    ``` 
    /projects/FaceDetector/V2> publish.set-destination.scoped .. gh:aryairani/face-detector
    	I will publish /projects/FaceDetector to gh:aryairani/face-detector
    /projects/FaceDetector/V2> publish
    
    	Syncing /projects/FaceDetector to gh:aryairani/face-detector
    	Syncing / to gh:aryairani/private-repo
    	
    /projects/FaceDetector/V2>
    ```
    
    Elsewhere:
    
    ``` 
    libs> clone gh:aryairani/face-detector FaceDetector
    libs> ls FaceDetector
    
      Dependencies.Abc.asdf : Blah -> Blah
      Dependencies.Abc.ghjk : Blah -> Blah
      Dependencies.Xyz.awww : Blah -> Blah
    	V1.result
    	...
    	V2.result
    	...
    libs>
    ```

# Sharing my code as library

TBD, but it will include:

  - specifying which code
  - specifying the publication destination
  - juggling some credentials for the destination

Next: [Updating my library & sharing an updated library](publishing-library2.md)

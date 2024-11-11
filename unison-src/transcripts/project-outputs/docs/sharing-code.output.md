# Sharing code

``` haskell
data Causal m e
  = One { currentHash :: Hash, head :: e }
  | Cons { currentHash :: Hash, head :: e, tail :: m (Causal e) }
  -- The merge operation `<>` flattens and normalizes for order
  | Merge { currentHash :: Hash, head :: e, tails :: Map Hash (m (Causal e)) }

-- just one level of name, like Foo or Bar, but not Foo.Bar
newtype NameSegment = NameSegment { toText :: Text } 
newtype Path = Path { toList :: [NameSegment] }

data Namespace m = Namespace
	{ terms :: Relation NameSegment Referent
  , types :: Relation NameSegment Reference
  , children :: Relation NameSegment (Codetree m) 
  }

data Codetree m = Codetree (Causal m Namespace)

data RemotePath = RemotePath RemoteRef Path
data RemoteRef = GithubRef { username :: Text, repo :: Text, treeish :: Text }
						-- | ...
-- "gh:<user>/<repo>[/<path>][?ref=<treeish>] -- treeish defaults to repo's `default_branch`
-- "gh:aryairani/unison/libs?ref=topic/370" becomes
-- RemotePath (GithubRef "aryairani" "unison" "topic/370") (Path ["libs"])

newtype EditMap = EditMap { toMap :: Map GUID (Causal Edits) }
data Edits = Edits
	{ terms :: Relation Reference TermEdit 
	, types :: Relation Reference TypeEdit
	}
	
-- maps local paths to remote paths
data RemoteStatus = Map Path RemoteSpec
```

A couple of important points:

  - A namespace is "just" part of your preferences for parsing (and to some extent, rendering) code.
  - Edits as we know them are just state for edit helper commands, like "todo" and "propagate"
  - We should consider making the codebase representation of this data modular, since they really can be separated; they are likely still meaningful even in the presence of unexpected state/preferences that might exist in the future to support other features of future versions of the editor.
  - We use `Causal` to represent a shareable data structure — shareable in the sense that which can tell whether a certain change came after another.

Questions:

  - Do we want to distinguish between `/` paths and `.` separators in names?
    
      - Should a type `A` be at the same level as
    
      - On one hand, you probably don't need to separate a type `A` from its constructor `A.A`.  You wouldn't be able to export the constructor without the type which resides a level up in the namespace.
    
      - Maybe the type `A` should organically be organized as `A/A`, and its constructor also as `A/A`.  This is reminiscent of having a separate module per type in Haskell, except that a reorganization could be done more easily:
        
        ``` 
        /mycode> mv ClassA* ClassA/
        /mycode> mv ClassB* ClassB/
        /mycode> cd ClassA
        /mycode/ClassA> ls
        ```
    
      - ``` 
        
        ```

## NameTree representation

examples:

``` 
<empty>

/A   (type)
/A   (term)
/A/A (ctor)



```

``` haskell
data NameTree a = Causal (Relation Name (NameTree a))
```

or

``` haskell
data NameTree a 
	= Leaf a 
	| Branch (Relation Name (NameTree a)) 
	| SharePoint (Causal (NameTree a))
```

## Github Notes

Base: https://api.github.com/repos/unisonweb/unison/

Branches: https://api.github.com/repos/unisonweb/unison/branches

A directory:

``` 
url: 
https://api.github.com/repos/unisonweb/unison/contents/unison-src/demo?ref=master

html_url: 
https://github.com/unisonweb/unison/tree/master/unison-src/demo

git_url
https://api.github.com/repos/unisonweb/unison/git/trees/f8d91c6cc2ee1bc8f2bfc759e328a851d0df3b95
```

A file:

``` 
url:
https://api.github.com/repos/unisonweb/unison/contents/unison-src/Base.u?ref=master

html_url:
https://github.com/unisonweb/unison/blob/master/unison-src/Base.u

git_url: https://api.github.com/repos/unisonweb/unison/git/blobs/e617fbad4e32d25380f536179f558f9213cd4bad

download_url:
https://raw.githubusercontent.com/unisonweb/unison/master/unison-src/Base.u
```

Note that `treeish` (in this example, `master`) can contain slashes, such as `topic/370`.  This makes parsing a little tricky.  Fortunately, if you have a git branch `a/b` then it's not possible to create branches `a` or `a/b/c`.  So you can load the list of branches, and then test them against that treeish-prefixed path:

`https://github.com/<user>/<repo>/<"tree" or "blob">/<treeish-prefixed-path>`

If any of the branch names + `/` form a prefix of `treeish-prefixed-path`, then the suffix is the path into the causal.  Crap, wait. The github HTML UI isn't going to be showing Unison paths at all.

So, we could use out made up `gh:username/repo[:treeish][/path]` URI scheme; can support others as desired.  Maybe our Javascript viewer will create URLs with query params that can indicate the Unison path.

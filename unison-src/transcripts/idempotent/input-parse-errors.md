# demonstrating our new input parsing errors

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison :hide
x = 55
```

``` ucm :hide
scratch/main> add
```

`handleNameArg` parse error in `add`

``` ucm :error
scratch/main> add .

  ⚠️

  Sorry, I wasn’t sure how to process your request:

    1:2:
      |
    1 | .
      |  ^
    unexpected end of input
    expecting '`' or operator (valid characters: !$%&*+-/:<=>\^|~)
    

  You can run `help add` for more information on using `add`.

scratch/main> ls

  1. lib/ (469 terms, 74 types)
  2. x    (Nat)

scratch/main> add 1


scratch/main> ls

  1. lib/ (469 terms, 74 types)
  2. x    (Nat)

scratch/main> add 2

  ⊡ Ignored previously added definitions: x
```

todo:

``` haskell
  SA.Name name -> pure name
  SA.NameWithBranchPrefix (Left _) name -> pure name
  SA.NameWithBranchPrefix (Right prefix) name -> pure $ Path.prefixNameIfRel (Path.AbsolutePath' prefix) name
  SA.HashQualified hqname -> maybe (Left "can’t find a name from the numbered arg") pure $ HQ.toName hqname
  SA.HashQualifiedWithBranchPrefix (Left _) hqname -> pure $ HQ'.toName hqname
  SA.HashQualifiedWithBranchPrefix (Right prefix) hqname ->
    pure . Path.prefixNameIfRel (Path.AbsolutePath' prefix) $ HQ'.toName hqname
  SA.ShallowListEntry prefix entry ->
    pure . HQ'.toName . fmap (Path.prefixNameIfRel prefix) $ shallowListEntryToHQ' entry
  SA.SearchResult mpath result ->
    maybe (Left "can’t find a name from the numbered arg") pure . HQ.toName $ searchResultToHQ mpath result
  otherNumArg -> Left . I.Formatted $ wrongStructuredArgument "a name" otherNumArg
```

aliasMany: skipped -- similar to `add`

``` ucm :error
scratch/main> update arg

  ⚠️

  Sorry, I wasn’t sure how to process your request:

    I expected no arguments, but received one.

  You can run `help update` for more information on using
  `update`.
```

aliasTerm

``` 
scratch/main> alias.term ##Nat.+ Nat.+
```

aliasTermForce,
aliasType,

todo:

``` 

aliasMany,
api,
authLogin,
back,
branchEmptyInputPattern,
branchInputPattern,
branchRenameInputPattern,
branchesInputPattern,
cd,
clear,
clone,
compileScheme,
createAuthor,
debugClearWatchCache,
debugDoctor,
debugDumpNamespace,
debugDumpNamespaceSimple,
debugTerm,
debugTermVerbose,
debugType,
debugLSPFoldRanges,
debugFileHashes,
debugNameDiff,
debugNumberedArgs,
debugTabCompletion,
debugFuzzyOptions,
debugFormat,
delete,
deleteBranch,
deleteProject,
deleteNamespace,
deleteNamespaceForce,
deleteTerm,
deleteTermVerbose,
deleteType,
deleteTypeVerbose,
deleteVerbose,
dependencies,
dependents,
diffNamespace,
display,
displayTo,
docToMarkdown,
docs,
docsToHtml,
edit,
editNamespace,
execute,
find,
findIn,
findAll,
findInAll,
findGlobal,
findShallow,
findVerbose,
findVerboseAll,
sfind,
sfindReplace,
forkLocal,
help,
helpTopics,
history,
ioTest,
ioTestAll,
libInstallInputPattern,
load,
makeStandalone,
mergeBuiltins,
mergeIOBuiltins,
mergeOldInputPattern,
mergeOldPreviewInputPattern,
mergeOldSquashInputPattern,
mergeInputPattern,
mergeCommitInputPattern,
names False, -- names
names True, -- names.global
namespaceDependencies,
previewAdd,
previewUpdate,
printVersion,
projectCreate,
projectCreateEmptyInputPattern,
projectRenameInputPattern,
projectSwitch,
projectsInputPattern,
pull,
pullWithoutHistory,
push,
pushCreate,
pushExhaustive,
pushForce,
quit,
releaseDraft,
renameBranch,
renameTerm,
renameType,
moveAll,
reset,
resetRoot,
runScheme,
saveExecuteResult,
test,
testAll,
todo,
ui,
undo,
up,
update,
updateBuiltins,
updateOld,
updateOldNoPatch,
upgrade,
upgradeCommitInputPattern,
view,
viewGlobal,
viewReflog
```

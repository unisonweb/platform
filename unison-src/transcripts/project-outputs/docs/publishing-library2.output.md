previous: [Using library code in my application & sharing my application as a library](publishing-library1.md)

# Updating my library & sharing the updates

We can imagine a number of potential classes of structured edits to the codebase, each requiring their own supporting data and DX design.  Like the ability to assign of names to references, these aren't fundamental to Unison; but they are critical to usability.

In designing our codebase representation, we should remember that **the set of edit helpers will likely change over time**. Although the codebase editor will have to change to support new edit classes, **the codebase format may not need to**.  i.e. each edit class could have some ID as part of its supporting data, and data for edit classes not supported by a particular codebase editor could be gracefully ignored.

## Replacement & deprecation of definitions

The first structured edit we've begun to tackle is: replacement and deprecation of definitions, propagated to dependents within the scope defined by a branch's namespace.

Currently, we accumulate "edit" directives as part of a branch:

``` haskell
editedTerms :: Branch0 -> Relation Reference TermEdit
editedTypes :: Branch0 -> Relation Reference TypeEdit
data TypeEdit = Replace Reference | Deprecate
data TermEdit = Replace Reference Typing | Deprecate
data Typing = Same | Subtype | Different
```

A relation `(r, edit)` indicates that we are working to remove `r` from the edit scope (currently: the branch `Namespace`).

  - These edits are simply metadata used by the `todo` and (unused/obsolete?) `propagate` commands.
  - These edits currently accumulate forever and are applied in perpetuity.
  - Edits are meant to be used to help users of a library to upgrade between versions, by describing how to rewrite their usage sites.

We are going to want to do some or all of the following:

  - Define/use short-term edits
  - Define edits within a limited set of code
  - Share with others how to upgrade their own dependents of our code, *in a way that allows them to understand what's going to happen and then choose to opt-in*.

### Short-term edits

We can quick-fix the "in perpetuity" part by giving the user an `edit.clear` command to "forget" an edit directive in a branch.  There are potentially a huge number of edits for the user to select among, but we can help a little with that by utilizing the same numbered-args scheme as `ls` currently uses, and/or by offering different ways of sorting: by name, by recency, other?

### Making managing edits manageable

If a human is meant to maintain this list by manually culling edit directives, he will need more context than a list of `Reference` pairs. e.g.: Where did this edit come from? Was it created by the `update` command on a .u file, or the \[likely not yet implemented\] `replace` command in the code editor? Or by auto-propagation? By whom? When? Other?  We should add at least a flag to indicate whether the update was manual or auto-propagated.  Maybe even a human-readable message:

``` haskell
data EditSource = ManualUpdate | ManualReplace | AutoPropagate
data EditReason = EditReason EditSource (Optional Text)
```

### Managing multiple sets of edits

Here is a hand-wavy, imagined script for managing multiple sets of edits:

``` 
master>
  ┌
  │  ✅
  │  
  │  I found and typechecked these definitions in Base.u:
  │  
  │    Sequence.map : c -> (a -> b) -> [a] -> [b]
  │  
  │  Now evaluating any watch expressions (lines starting with `>`)...
  └
master> edit.set-reason adding a silly parameter to Sequence.map
master> update
  ┌
  │  ✅
  │  
  │  I updated these definitions as part of "adding a silly parameter to
  │    Sequence".
  │  
  │    Sequence.map : c -> (a -> b) -> [a] -> [b]
  └
master> edit.list

 "adding a silly parameter to sequence":
    Terms:
    Sequence.map#31q -> Sequence.map
    Sequence.map#aa4 -> Sequence.map#31q

master> edit.elide Sequence.map#31q

  You still have 6 dependents of Sequence.map#31q in this branch.

  Repeat the same command to proceed anyway.

  Tip: Use `todo` to see what's left to do in the refactor.

  Tip: Use `edit.clear Sequence.map#31q` to cancel refactoring its dependents.

master> edits.save Sequence.wip20190315

  2 edits saved as Sequence.wip20190315

master> edit.elide Sequence.map#31q

  You still have 6 dependents <snip> repeat the same command to proceed anyway.

master> edit.elide Sequence.map#31q

  Cleared:
    Sequence.map#31q -> Sequence.map
    Sequence.map#aa4 -> Sequence.map#31q

  Added:
    Sequence.map#aa4 -> Sequence.map


master> edits.save Sequence.upgrade20190315

  1 edit saved as Sequence.ugprade20190315

master> publish gh:aryairani/Sequence:sequence

  Pushed 2 new definitions to gh:aryairani/Sequence/sequence

master>^C
```

Then, elsewhere:

``` haskell
import gh:aryairani/Sequence:master/Sequence as Sequence
```

``` 
master> add
```

``` 
master> edits.activate git:runarorama/Multiset/Multiset.upgrade2_3

  Activated 6 edit directives.

  Your branch has 37 affected dependents, 35 of which can be upgraded automatically.

  Tip: Use `view git:runarorama/Multiset/Multiset.upgrade2_3` to summarize the changes.

  Tip: Use `todo` to see what's left to complete these edits.

master> todo
```

### First-class edits

An edit set could be represented by a Unison term.  The previous example is meant to be ambiguous as to whether or not that is the case, but it could be, and I suspect

### How do we manage secondary edits?

Working through one set of edits/upgrades produces a secondary set of edits.  Where, if anywhere should this secondary set be saved long-term?  What effect will it have on bookkeeping if a user wants to process more than one first-class edit sets at the same time? i.e. in the course of processing updates from library Foo to library Foo', and library Bar to library Bar', if I update App.func1 to App.func1', to which library update can I attribute that change?  Well, we haven't discussed anything about attributing application changes to library changes, but

## Curating edits

The user should be able to curate the list of edits that are in the branch, like what we do when auditing an unsubmitted Github PR.  The example script in the earlier section explores this a bit, but if the edit lists could be edited in the `.u`, or  by Unison code at some point in the future, that will probably be much more convenient than implementing a ton of CLI commands to manipulate the list(s).

### Curating name changes

Could the branch/namespace also be a first-class Unison term?  How would that ground out?

## Publishing a set of edits

If a set of edits is just a Unison term that the CLI knows about, then you can publish it in the same way you publish unison terms; TBD once we confirm the branch/repo format.

## Using an updated library

The example above touched on this in the example above, with

``` 
> edits.activate gh:runarorama/Multiset/Multiset.upgrade2_3
```

or, having linked `/libs/Multiset` to `gh:runarorama/Multiset/...`:

``` 
> edits.activate /libs/Multiset/upgrade2_3
```

We can collect additional questions here.

<!-- [Upgrading my application with an updated library](publishing-library3.md) -->

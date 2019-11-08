# Documenting Unison code

Unison documentation is written in Unison. Documentation is a value of the following type:

```ucm
.> view builtin.Doc

  unique type builtin.Doc
    = Link builtin.Link
    | Source builtin.Link
    | Blob builtin.Text
    | Join [builtin.Doc]
    | Signature builtin.Link.Term
    | Evaluate builtin.Link.Term

```
You can create these `Doc` values with ordinary code, or you can use the special syntax. A value of type `Doc` can be created via syntax like:

```unison
use .builtin

doc1 = [: This is some documentation.

It can span multiple lines.

Can link to definitions like @List.drop or @List

:]
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      doc1 : builtin.Doc
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
Syntax:

`[:` starts a documentation block; `:]` finishes it. Within the block:

* Links to definitions are done with `@List`. `\@` if you want to escape.
* `@[signature] List.take` expands to the type signature of `List.take`
* `@[source] List.map` expands to the full source of `List.map`
* `@[include] someOtherDoc`, inserts a value `someOtherDoc : Doc` here.
* `@[evaluate] someDefinition` expands to the result of evaluating `someDefinition`, which must be a pre-existing definition in the codebase (can't be an arbitrary expression).

### An example

We are going to document `List.take` using some verbiage and a few examples. First we have to add the examples to the codebase:

```unison
List.take.ex1 = take 0 [1,2,3,4,5]
List.take.ex2 = take 2 [1,2,3,4,5]
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      List.take.ex1 : [builtin.Nat]
      List.take.ex2 : [builtin.Nat]
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    List.take.ex1 : [builtin.Nat]
    List.take.ex2 : [builtin.Nat]

```
And now let's write our docs and reference these examples:

```unison
use .builtin

docs.List.take = [:

`@List.take n xs` returns the first `n` elements of `xs`. (No need to add line breaks manually. The display command will do wrapping of text for you.)

## Examples:

@[source] List.take.ex1
🔽
@[evaluate] List.take.ex1


@[source] List.take.ex2
🔽
@[evaluate] List.take.ex2
:]
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      docs.List.take : builtin.Doc
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
Let's add it to the codebase, and link it to the definition:

```ucm
.> add

  ⍟ I've added these definitions:
  
    docs.List.take : builtin.Doc

.> link builtin.List.take docs.List.take

  Done.

```
Now that documentation is linked to the definition. We can view it if we like:

```ucm
.> links builtin.List.take builtin.Doc

  1. docs.List.take : builtin.Doc
  
  Tip: Try using `display 1` to display the first result or
       `view 1` to view its source.

.> display 1

  `builtin.List.take n xs` returns the first `n` elements of
  `xs`. (No need to add line breaks manually. The display
  command will do wrapping of text for you.)
  
  ## Examples:
  
  List.take.ex1 = builtin.List.take 0 [1, 2, 3, 4, 5]
  🔽
  List.take.ex1 = []
  
  
  List.take.ex2 = builtin.List.take 2 [1, 2, 3, 4, 5]
  🔽
  List.take.ex2 = [1, 2]

```
Or there's also a convenient function, `docs`, which shows the `Doc` values that are linked to a definition. It's implemented in terms of `links` and `display`:

```ucm
.> docs builtin.List.take

  `builtin.List.take n xs` returns the first `n` elements of
  `xs`. (No need to add line breaks manually. The display
  command will do wrapping of text for you.)
  
  ## Examples:
  
  List.take.ex1 = builtin.List.take 0 [1, 2, 3, 4, 5]
  🔽
  List.take.ex1 = []
  
  
  List.take.ex2 = builtin.List.take 2 [1, 2, 3, 4, 5]
  🔽
  List.take.ex2 = [1, 2]

```

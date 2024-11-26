# Design for Unison documentation and comments

This is a rough design of a way to supply commentary and formal documentation for Unison code. [Discuss here](https://github.com/unisonweb/unison/issues/462) and also be sure to view the raw markdown file for some embedded comments.

## Comments

Comments in Unison can be either line comments or block comments. It’s probably only necessary to implement one of these for a first release of Unison, but ultimately we may want to offer both.

<!--
 Sounds good, should have both.
 Arya: how do you add docs after the fact?
  -->

### Line comments

Line comments can be introduced in code with a special token. For example, if we want Haskell-like syntax, the `--` token introduces a comment:

``` 
foo x y = 
  -- This is a comment
  x + y
```

Line comments follow these syntactic rules:

1.  A line comment must occupy the whole line. For simplicity, it’s a syntax error to put a comment at the end of a line that contains anything other than whitespace.
2.  The comment is attached to the abstract syntax tree node that is BEGUN by the token following the comment.
3.  When rendering comments, the indentation should be the same as the token that follows the comment.

<!--
Sounds good.

Question: are comments just text? Runar: yes. Paul: maybe they should be more structured? Allow hyperlinks?

No for now. Most of the time, comments are really unstructured anyway, and we can evolve the comment type later if it turns out that's super useful.

Could also have a renderer for these comments that interprets the text as markdown or something.
  -->

### Block comments

Block comments can be introduced with special brackets. For example, if we want Haskell-like syntax, the `{-``-}` brackets delimit a block comment:

``` 
foo x y = 
  {- This is a comment. -} x + y

foo x y = {- comment -} (x + y)

foo x y = 
  {- comment -}
  (x + y)

foo x y = 
  {- comment -}
  x + y
```

Block comments follow these syntactic rules:

1.  A block comment can appear anywhere.
2.  The comment is attached to the abstract syntax tree node that is BEGUN by the token following the comment. If that's not defined, could be an error, or could just use some ad hoc heuristic to find "nearest" AST node.
3.  When rendering comments, the indentation should be the same as the token that follows the comment.

<!-- 
Question: what exactly is the grammar and how is it parsed? Just some details to work out here.
-->

### Comments and code structure

Comments should not have any effect on the hash of a Unison term or type. I propose that comments be kept as an annotation on the AST rather than as part of the AST itself. This way, comments can be edited, added, or removed, without touching the AST.

<!-- 
I like this idea a lot, multiple people can comment on the same definition in different ways!

Question: how do you pick which comments are rendered when viewing a definition? (If there multiple sets of comments?)
Feels like comments should have metadata ("Runar wrote this comment, on this date, in Icelandic").

Having a more comprehensive, extensible metadata design would solve this.
-->

### Comments and the codebase

Comments should be stored in the codebase as annotations on the syntax tree. For example, under the hash for the term (or type), we could add a new file `comments.ub` that contains the comments in pairs of `(AST node index, comment text)`.

A future version might allow for multiple comment sets (commentary with different purposes or audiences) by adding e.g. a tag field to the comments, or having a whole `comments` directory instead of just one file.

<!-- 
Seems good, key is that comments are attached to AST node, question is how do you refer to a specific AST node? Probably some sort of root to leaf path.

Should all the comments be in one file? In separate files? To avoid git merges, the file has to be called `<hash>.comments.ub` or something. And then the code viewer will look up all the `.comments` for a definition and let you pick one or more based on metadata something something.
--> 

## API documentation

Any hash in the codebase can have formal API documentation associated with it. This might include basic usage, free-text explanations, examples, links to further reading, and links to related hashes.

Probably some flavor of Markdown is ideal for API docs.

<!-- 
Sounds good.

What about things like examples and doctests?

``` Unison:example
sort by stuff = ...

> sort (<) [2,3,1,2,4]
```

So in a `Unison:example` block, the watch expressions are evaluated at documentation parse time and are shown when viewing the docs for the definition. Should just get normal type errors and whatnot if the examples don't work.

For doctests you can include the source of an existing test and have its result displayed as an example:

```Unison:example
{transclude: tests.ex1 :}
```

Q: if an example block has definitions in it that aren't watches, what happens to those? Do we need to add it to the codebase? We can get away with just not adding to the codebase.

Can just concatenate all the example blocks into one big block, evaluate all at once, allowing multiple examples to reference common helper functions.

Links to further reading - just use a section for this, with links in it, as a convention.
-->

### The Unison CLI and API docs

Ultimately we’ll want to have a more visual codebase editor (see e.g. Pharo Smalltalk), but for now we have the Unison CLI. So there ought to be a special syntax for indicating that you want to associate API docs to a definition when you `add` it to the codebase (or `update`). This syntax should be light-weight and easy to type.

For example:

``` 
{| `foo x y` adds `x` to `y` |}

foo x y = x + y
```

The rule here would be that the documentation block gets associated with the definition that immediately follows.

Alternatively, something like:

``` 
{foo| `foo x y` adds `x` to `y`|}.
```

<!-- 
I like that you can add API docs later to a definition.

For docbase documentation, nothing special needed, just write a new docbase page that references existing definitions. Unison can surface these "tracebacks" automatically.
-->

This would associate the documentation block to the hash named `foo`  even if that hash isn’t being otherwise edited in the file.

### Semantic content of API docs

Wherever docs have code (in Markdown between fences or backticks), Unison should parse that code, resolve names, and substitute hashes for names.

E.g., the doc might have a usage example:

``` 
{|
Usage: `foo x y` adds `x` to `y`.
|}
```

When this doc block gets processed by Unison, it should parse `foo x y` and recognize that `foo`, `x`, and `y` are free. It should replace `foo` with a hyperlink to the hash of `foo`. It should do this for every name that exists in the codebase.

There should be some syntax to exclude a code block from this processing.

Alternatively, we could have special syntax to indicate that something should be parsed as a Unison name. E.g.

``` 
{| 
Usage: `@foo x y` adds `x` to `y`.
|}
```

Where `@foo` indicates that `foo` is a Unison name, we’d like an error if it isn’t, and it should be replaced in the rendered docs with a hyperlink to `foo`.

### Opinionated doc format

It’s possible that we’ll want to be very opinionated about how what goes into API documentation, for uniformity across libraries and ease of use.

For example, we might have API docs support the following fields for a function definition:

  - Usage: How to call the function. E.g. “`foo x y` adds `x` to `y`”. We should maintain the invariant that the usage is correct (that it matches the name of the function and its arity).
  - Examples: discussed above.

Note that author name, time stamp, etc, can be inferred from the codebase. These are data that can be displayed in the API docs when rendered, but don’t need to be written by the author.

<!-- Like that other metadata is just known to Unison and can displayed or not. -->

## Docbase/Wiki

Separately from API documentation, it would be good to be able to write tutorials or long-form explanations of Unison libraries, with links into the codebase API docs.

We’d need to write a tool that can process e.g. Github-flavoured Markdown together with a Unison codebase. The markdown format would have Unison-specific extensions to allow hyperlinking Unison hashes as well as Tut-style evaluation of examples.

Ideally, the documentation would be kept automatically up to date in the face of renames, etc.

Processing has to have two distinct phases, authoring and rendering.

  - *Authoring*: you write the markdown document and use Unison human-readable names in your code. When you add your document to the docbase, all the names get replaced with Unison hashes before being stored.
  - *Rendering*: A document stored in the docbase could then be rendered as e.g. HTML (or Markdown) where Unison hashes are turned back to human-readable names from the codebase, and hyperlinked to the API documentation for the hashes.

<!-- 
How is this stored? Maybe docs are first-class, just like any other definition. If I'm documenting `foo`, some of its dependents could be documentation values.

Will need metadata system to be able to pick out docs for a definition, otherwise no changes to codebase format.

Is the docs / API docs builtin types?
-->

### Transclusion

A particularly useful feature for this kind of documentation tool would be *transclusion* of code. E.g. with a syntax like…

``` 
{:transclude MyLibrary.myFun}
```

The tool could render that as a code block containing the definition of `MyLibrary.myFun`. Ideally that would register this document as a dependency of `MyLibrary.myFun` and propagation of updates could work the same way as for code.

It would be good to also have a way (as in Elm) of transcluding the API docs of individual types and functions in a document.

This is a way of keeping documentation automatically up to date, at least partially.

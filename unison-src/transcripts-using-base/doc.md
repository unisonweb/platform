# Computable documents in Unison

Unison documentation is written in Unison and has some neat features:

* The documentation type provides a rich vocabulary of elements that go beyond markdown, including asides, callouts, tooltips, and more.
* Docs may contain Unison code which is parsed and typechecked to ensure validity. No more out of date examples that don't compile or assume a bunch of implicit context!
* Embeded examples are live and can show the results of evaluation. This uses the same evaluation cache as Unison's scratch files, allowing Unison docs to function like well-commented spreadsheets or notebooks.
* Links to other definitions are typechecked to ensure they point to valid definitions. The links are resolved to hashes and won't be broken by name changes or moving definitions around.
* Docs can be included in other docs and you can assemble documentation programmatically, using Unison code.
* There's a powerful textual syntax for all of the above, which we'll introduce next.

## Introduction

Documentation blocks start with `{{` and end with a matching `}}`. You can introduce doc blocks anywhere you'd use an expression, and you can also have anonymous documentation blocks immediately before a top-level term or type.

``` unison
name = {{Alice}}
d1 = {{ Hello there {{name}}! }}

{{ An important constant, equal to @eval{ImportantConstant} }}
ImportantConstant = 41 + 1

{{
The 7 days of the week, defined as:

  @source{type DayOfWeek}
}}
unique type time.DayOfWeek = Sun | Mon | Tue | Wed | Thu | Fri | Sat
```

Notice that an anonymous documentation block `{{ ... }}` before a definition `ImportantConstant` is just syntax sugar for `ImportantConstant.doc = {{ ... }}`.

You can preview what docs will look like when rendered to the console using the `display` or `docs` commands:

``` ucm
scratch/main> display d1
scratch/main> docs ImportantConstant
scratch/main> docs DayOfWeek
```

The `docs ImportantConstant` command will look for `ImportantConstant.doc` in the file or codebase. You can do this instead of explicitly linking docs to definitions.

## Syntax guide

First, we'll load the `syntax.u` file which has examples of all the syntax:

``` ucm
scratch/main> load "./unison-src/transcripts-using-base/doc.md.files/syntax.u"
```

``` ucm :hide
scratch/main> add
```

Now we can review different portions of the guide.
we'll show both the pretty-printed source using `view`
and the rendered output using `display`:

``` ucm
scratch/main> view basicFormatting
scratch/main> display basicFormatting
scratch/main> view lists
scratch/main> display lists
scratch/main> view evaluation
scratch/main> display evaluation
scratch/main> view includingSource
scratch/main> display includingSource
scratch/main> view nonUnisonCodeBlocks
scratch/main> display nonUnisonCodeBlocks
scratch/main> view otherElements
scratch/main> display otherElements
```

Lastly, it's common to build longer documents including subdocuments via `{{ subdoc }}`. We can stitch together the full syntax guide in this way:

``` ucm
scratch/main> view doc.guide
scratch/main> display doc.guide
```

🌻 THE END

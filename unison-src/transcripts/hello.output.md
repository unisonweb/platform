# Hello\!

This markdown file is also a Unison transcript file. Transcript files are an easy way to create self-documenting Unison programs, libraries, and tutorials.

The format is just a regular markdown file with some fenced code blocks that are typechecked and elaborated by `ucm`. For example, you can call this transcript via:

``` 
$ ucm transcript hello.md
```

This runs it on a freshly generated empty codebase. Alternately `ucm transcript.fork --codebase /path/to/code hello.md` runs the transcript on a freshly generated copy of the provided codebase. Do `ucm help` to learn more about usage.

Fenced code blocks of type `unison` and `ucm` are treated specially:

  - `ucm` blocks are executed, and the output is interleaved into the output markdown file after each command, replacing the original `ucm` block.
  - `unison` blocks are typechecked, and a `ucm` block with the output of typechecking and execution of the file is inserted immediately afterwards.

Take a look at [the elaborated output](hello.output.md) to see what this file looks like after passing through the transcript runner.

## Let's try it out\!\!

In the `unison` fenced block, you can give an (optional) file name (defaults to `scratch.u`), like so:

``` unison
---
title: myfile.u
---
x = 42

```

``` ucm

  Loading changes detected in myfile.u.

  I found and typechecked these definitions in myfile.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      x : Nat

```
Let's go ahead and add that to the codebase, then make sure it's there:

``` ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    x : Nat

scratch/main> view x

  x : Nat
  x = 42

```
If `view` returned no results, the transcript would fail at this point.

## Hiding output

You may not always want to view the output of typechecking and evaluation every time, in which case, you can add `:hide` to the block. For instance:

``` unison
y = 99
```

This works for `ucm` blocks as well.

Doing `unison :hide:all` hides the block altogether, both input and output - this is useful for doing behind-the-scenes control of `ucm`'s state.

## Expecting failures

Sometimes, you have a block which you are *expecting* to fail, perhaps because you're illustrating how something would be a type error. Adding `:error` to the block will check for this. For instance, this program has a type error:

``` unison
hmm : .builtin.Nat
hmm = "Not, in fact, a number"
```

``` ucm

  Loading changes detected in scratch.u.

  I found a value of type:   Text
  where I expected to find:  Nat
  
      1 | hmm : .builtin.Nat
      2 | hmm = "Not, in fact, a number"
  
    from right here:
  
      2 | hmm = "Not, in fact, a number"
  

```

This transcript explains a few minor details about doc parsing and pretty-printing, both from a user point of view and with some implementation notes.  The later stuff is meant more as unit testing than for human consumption.  (The ucm `add` commands and their output are hidden for brevity.)

Docs can be used as inline code comments.

``` ucm :hide
scratch/main> builtins.merge
```

``` unison
foo : Nat -> Nat
foo n =
  _ = [: do the thing :]
  n + 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      foo : Nat -> Nat
```

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> view foo

  foo : Nat -> Nat
  foo n =
    use Nat +
    _ = [: do the thing :]
    n + 1
```

Note that `@` and `:]` must be escaped within docs.

``` unison
escaping = [: Docs look [: like \@this \:] :]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      escaping : Doc
```

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> view escaping

  escaping : Doc
  escaping = [: Docs look [: like \@this \:] :]
```

(Alas you can't have `\@` or `\:]` in your doc, as there's currently no way to 'unescape' them.)

``` unison
-- Note that -- comments are preserved within doc literals.
commented = [:
  example:

    -- a comment
    f x = x + 1
:]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      commented : Doc
```

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> view commented

  commented : Doc
  commented =
    [: example:
    
    -- a comment f x = x + 1
     :]
```

### Indenting, and paragraph reflow

Handling of indenting in docs between the parser and pretty-printer is a bit fiddly.

``` unison
-- The leading and trailing spaces are stripped from the stored Doc by the
-- lexer, and one leading and trailing space is inserted again on view/edit
-- by the pretty-printer.
doc1 = [:   hi   :]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      doc1 : Doc
```

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> view doc1

  doc1 : Doc
  doc1 = [: hi :]
```

``` unison
-- Lines (apart from the first line, i.e. the bit between the [: and the
-- first newline) are unindented until at least one of
-- them hits the left margin (by a post-processing step in the parser).
-- You may not notice this because the pretty-printer indents them again on
-- view/edit.
doc2 = [: hello
            - foo
            - bar
          and the rest. :]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      doc2 : Doc
```

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> view doc2

  doc2 : Doc
  doc2 =
    [: hello
      - foo
      - bar
    and the rest. :]
```

``` unison
doc3 = [: When Unison identifies a paragraph, it removes any newlines from it before storing it, and then reflows the paragraph text to fit the display window on display/view/edit.

For these purposes, a paragraph is any sequence of non-empty lines that have zero indent (after the unindenting mentioned above.)

 - So this is not a paragraph, even
   though you might want it to be.

   And this text  | as a paragraph
   is not treated | either.

Note that because of the special treatment of the first line mentioned above, where its leading space is removed, it is always treated as a paragraph.
   :]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      doc3 : Doc
```

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> view doc3

  doc3 : Doc
  doc3 =
    [: When Unison identifies a paragraph, it removes any 
    newlines from it before storing it, and then reflows the 
    paragraph text to fit the display window on 
    display/view/edit.
    
    For these purposes, a paragraph is any sequence of non-empty
    lines that have zero indent (after the unindenting mentioned
    above.)
    
     - So this is not a paragraph, even
       though you might want it to be.
    
       And this text  | as a paragraph
       is not treated | either.
    
    Note that because of the special treatment of the first line
    mentioned above, where its leading space is removed, it is 
    always treated as a paragraph.
    :]
```

``` unison
doc4 = [: Here's another example of some paragraphs.

          All these lines have zero indent.

            - Apart from this one. :]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      doc4 : Doc
```

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> view doc4

  doc4 : Doc
  doc4 =
    [: Here's another example of some paragraphs.
    
    All these lines have zero indent.
    
      - Apart from this one. :]
```

``` unison
-- The special treatment of the first line does mean that the following
-- is pretty-printed not so prettily.  To fix that we'd need to get the
-- lexer to help out with interpreting doc literal indentation (because
-- it knows what columns the `[:` was in.)
doc5 = [:   - foo
            - bar
          and the rest. :]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      doc5 : Doc
```

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> view doc5

  doc5 : Doc
  doc5 =
    [: - foo
      - bar
    and the rest. :]
```

``` unison
-- You can do the following to avoid that problem.
doc6 = [:
            - foo
            - bar
          and the rest.
       :]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      doc6 : Doc
```

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> view doc6

  doc6 : Doc
  doc6 =
    [: - foo
      - bar
    and the rest.
     :]
```

### More testing

``` unison
-- Check empty doc works.
empty = [::]

expr = foo 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      empty : Doc
      expr  : Nat
```

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> view empty

  empty : Doc
  empty = [:  :]
```

``` unison
test1 = [:
The internal logic starts to get hairy when you use the \@ features, for example referencing a name like @List.take.  Internally, the text between each such usage is its own blob (blob ends here --> @List.take), so paragraph reflow has to be aware of multiple blobs to do paragraph reflow (or, more accurately, to do the normalization step where newlines with a paragraph are removed.)

Para to reflow: lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor ending in ref @List.take

@List.take starting para lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor.

Middle of para: lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor @List.take lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor.

  - non-para line (@List.take) with ref @List.take
  Another non-para line
  @List.take starting non-para line

  - non-para line with ref @List.take
before a para-line lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor.

  - non-para line followed by a para line starting with ref
@List.take lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor.

a para-line ending with ref lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor @List.take
  - non-para line

para line lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor
  @List.take followed by non-para line starting with ref.

@[signature] List.take

@[source] foo

@[evaluate] expr

@[include] doc1

-- note the leading space below
  @[signature] List.take

:]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      test1 : Doc
```

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> view test1

  test1 : Doc
  test1 =
    [: The internal logic starts to get hairy when you use the 
    \@ features, for example referencing a name like @List.take.
    Internally, the text between each such usage is its own blob
    (blob ends here --> @List.take), so paragraph reflow has to 
    be aware of multiple blobs to do paragraph reflow (or, more 
    accurately, to do the normalization step where newlines with
    a paragraph are removed.)
    
    Para to reflow: lorem ipsum dolor lorem ipsum dolor lorem 
    ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum 
    dolor lorem ipsum dolor ending in ref @List.take
    
    @List.take starting para lorem ipsum dolor lorem ipsum dolor
    lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem 
    ipsum dolor lorem ipsum dolor.
    
    Middle of para: lorem ipsum dolor lorem ipsum dolor lorem 
    ipsum dolor @List.take lorem ipsum dolor lorem ipsum dolor 
    lorem ipsum dolor lorem ipsum dolor.
    
      - non-para line (@List.take) with ref @List.take
      Another non-para line
      @List.take starting non-para line
    
      - non-para line with ref @List.take
    before a para-line lorem ipsum dolor lorem ipsum dolor lorem
    ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum 
    dolor lorem ipsum dolor lorem ipsum dolor.
    
      - non-para line followed by a para line starting with ref
    @List.take lorem ipsum dolor lorem ipsum dolor lorem ipsum 
    dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor 
    lorem ipsum dolor lorem ipsum dolor.
    
    a para-line ending with ref lorem ipsum dolor lorem ipsum 
    dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor 
    lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor @List.take
      - non-para line
    
    para line lorem ipsum dolor lorem ipsum dolor lorem ipsum 
    dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor 
    lorem ipsum dolor lorem ipsum dolor
      @List.take followed by non-para line starting with ref.
    
    @[signature] List.take
    
    @[source] foo
    
    @[evaluate] expr
    
    @[include] doc1
    
    -- note the leading space below
      @[signature] List.take
    
    :]
```

``` unison
-- Regression test for #1363 - preservation of spaces after @ directives in first line when unindenting
reg1363 = [: `@List.take foo` bar
  baz :]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      reg1363 : Doc
```

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> view reg1363

  reg1363 : Doc
  reg1363 = [: `@List.take foo` bar baz :]
```

``` unison
-- Demonstrate doc display when whitespace follows a @[source] or @[evaluate]
-- whose output spans multiple lines.

test2 = [:
  Take a look at this:
  @[source] foo    ▶    bar
:]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      test2 : Doc
```

``` ucm :hide
scratch/main> add
```

View is fine.

``` ucm
scratch/main> view test2

  test2 : Doc
  test2 =
    [: Take a look at this:
    @[source] foo    ▶    bar
     :]
```

But note it's not obvious how display should best be handling this.  At the moment it just does the simplest thing:

``` ucm
scratch/main> display test2

  Take a look at this:
  foo : Nat -> Nat
  foo n =
    use Nat +
    _ = [: do the thing :]
    n + 1    ▶    bar
```

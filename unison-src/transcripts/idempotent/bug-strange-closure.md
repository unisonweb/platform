``` ucm :hide
scratch/main> builtins.mergeio lib.builtins

scratch/main> load "unison-src/transcripts-using-base/doc.md.files/syntax.u"
```

We can display the guide before and after adding it to the codebase:

```` ucm
scratch/main> display doc.guide

  # Unison computable documentation

    # Basic formatting
    
      Paragraphs are separated by one or more blanklines.
      Sections have a title and 0 or more paragraphs or other
      section elements.
    
      Text can be bold, *italicized*, ~~strikethrough~~, or
      `monospaced` (or `monospaced`).
    
      You can link to Unison terms, types, and external URLs:
    
      * An external url
      * Some is a term link; Optional is a type link
      * A named type link and a named term link. Term links are
        handy for linking to other documents!
    
      You can use `{{ .. }}` to escape out to regular Unison
      syntax, for instance __not bold__. This is useful for
      creating documents programmatically or just including
      other documents.
    
      *Next up:* lists

    # Lists
    
      # Bulleted lists
      
        Bulleted lists can use `+`, `-`, or `*` for the bullets
        (though the choice will be normalized away by the
        pretty-printer). They can be nested, to any depth:
      
        * A
        * B
        * C
          * C1
          * C2
    
      # Numbered lists
      
        1. A
        2. B
        3. C
      
        The first number of the list determines the starting
        number in the rendered output. The other numbers are
        ignored:
      
        10. A
        11. B
        12. C
      
        Numbered lists can be nested as well, and combined with
        bulleted lists:
      
        1. Wake up.
           * What am I doing here?
           * In this nested list.
        2. Take shower.
        3. Get dressed.

    # Evaluation
    
      Expressions can be evaluated inline, for instance `2`.
    
      Blocks of code can be evaluated as well, for instance:
    
          id x = x
          id (sqr 10)
          ⧨
          100
    
      also:
    
          match 1 with
            1 -> "hi"
            _ -> "goodbye"
          ⧨
          "hi"
    
      To include a typechecked snippet of code without
      evaluating it, you can do:
    
          use Nat *
          cube : Nat -> Nat
          cube x = x * x * x

    # Including Unison source code
    
      Unison definitions can be included in docs. For instance:
    
          structural type Optional a = Some a | None
          
          sqr : Nat -> Nat
          sqr x =
            use Nat *
            x * x
    
      Some rendering targets also support folded source:
    
          structural type Optional a = Some a | None
          
          sqr : Nat -> Nat
          sqr x =
            use Nat *
            x * x
    
      You can also include just a signature, inline, with
      `sqr : Nat -> Nat`, or you can include one or more
      signatures as a block:
    
          sqr : Nat -> Nat
      
          Nat.+ : Nat -> Nat -> Nat
    
      Or alternately:
    
          List.map : (a ->{e} b) -> [a] ->{e} [b]
    
      # Inline snippets
      
        You can include typechecked code snippets inline, for
        instance:
      
        * `f x Nat.+ sqr 1` - the `2` says to ignore the first
          two arguments when rendering. In richer renderers, the
          `sqr` link will be clickable.
        * If your snippet expression is just a single function
          application, you can put it in double backticks, like
          so: `sqr x`. This is equivalent to `sqr x`.

    # Non-Unison code blocks
    
      Use three or more single quotes to start a block with no
      syntax highlighting:
    
      ``` raw
         _____     _             
        |  |  |___|_|___ ___ ___ 
        |  |  |   | |_ -| . |   |
        |_____|_|_|_|___|___|_|_|
        
      ```
    
      You can use three or more backticks plus a language name
      for blocks with syntax highlighting:
    
      ``` Haskell
      -- A fenced code block which isn't parsed by Unison
      reverse = foldl (flip (:)) []
      ```
    
      ``` Scala
      // A fenced code block which isn't parsed by Unison
      def reverse[A](xs: List[A]) = 
        xs.foldLeft(Nil : List[A])((acc,a) => a +: acc)
      ```

    There are also asides, callouts, tables, tooltips, and more.
    These don't currently have special syntax; just use the
    `{{ }}` syntax to call these functions directly.
    
        docAside : Doc2 -> Doc2
    
        docCallout : Optional Doc2 -> Doc2 -> Doc2
    
        docBlockquote : Doc2 -> Doc2
    
        docTooltip : Doc2 -> Doc2 -> Doc2
    
        docTable : [[Doc2]] -> Doc2
    
    This is an aside. (
    Some extra detail that doesn't belong in main text. )
    
      | This is an important callout, with no icon.
    
      | 🌻
      | 
      | This is an important callout, with an icon. The text
      | wraps onto multiple lines.
    
    > "And what is the use of a book," thought Alice, "without
    > pictures or conversation?"
    > 
    > *Lewis Carroll, Alice's Adventures in Wonderland*
    
    Hover over me
    
    a           b           A longer paragraph that will split
                            onto multiple lines, such that this
                            row occupies multiple lines in the
                            rendered table.
    Some text   More text   Zounds!

scratch/main> add

  ⍟ I've added these definitions:

    basicFormatting     : Doc2
    doc.guide           : Doc2
    evaluation          : Doc2
    includingSource     : Doc2
    lists               : Doc2
    nonUnisonCodeBlocks : Doc2
    otherElements       : Doc2
    sqr                 : Nat -> Nat

scratch/main> display doc.guide

  # Unison computable documentation

    # Basic formatting
    
      Paragraphs are separated by one or more blanklines.
      Sections have a title and 0 or more paragraphs or other
      section elements.
    
      Text can be bold, *italicized*, ~~strikethrough~~, or
      `monospaced` (or `monospaced`).
    
      You can link to Unison terms, types, and external URLs:
    
      * An external url
      * Some is a term link; Optional is a type link
      * A named type link and a named term link. Term links are
        handy for linking to other documents!
    
      You can use `{{ .. }}` to escape out to regular Unison
      syntax, for instance __not bold__. This is useful for
      creating documents programmatically or just including
      other documents.
    
      *Next up:* lists

    # Lists
    
      # Bulleted lists
      
        Bulleted lists can use `+`, `-`, or `*` for the bullets
        (though the choice will be normalized away by the
        pretty-printer). They can be nested, to any depth:
      
        * A
        * B
        * C
          * C1
          * C2
    
      # Numbered lists
      
        1. A
        2. B
        3. C
      
        The first number of the list determines the starting
        number in the rendered output. The other numbers are
        ignored:
      
        10. A
        11. B
        12. C
      
        Numbered lists can be nested as well, and combined with
        bulleted lists:
      
        1. Wake up.
           * What am I doing here?
           * In this nested list.
        2. Take shower.
        3. Get dressed.

    # Evaluation
    
      Expressions can be evaluated inline, for instance `2`.
    
      Blocks of code can be evaluated as well, for instance:
    
          id x = x
          id (sqr 10)
          ⧨
          100
    
      also:
    
          match 1 with
            1 -> "hi"
            _ -> "goodbye"
          ⧨
          "hi"
    
      To include a typechecked snippet of code without
      evaluating it, you can do:
    
          use Nat *
          cube : Nat -> Nat
          cube x = x * x * x

    # Including Unison source code
    
      Unison definitions can be included in docs. For instance:
    
          structural type Optional a = Some a | None
          
          sqr : Nat -> Nat
          sqr x =
            use Nat *
            x * x
    
      Some rendering targets also support folded source:
    
          structural type Optional a = Some a | None
          
          sqr : Nat -> Nat
          sqr x =
            use Nat *
            x * x
    
      You can also include just a signature, inline, with
      `sqr : Nat -> Nat`, or you can include one or more
      signatures as a block:
    
          sqr : Nat -> Nat
      
          Nat.+ : Nat -> Nat -> Nat
    
      Or alternately:
    
          List.map : (a ->{e} b) -> [a] ->{e} [b]
    
      # Inline snippets
      
        You can include typechecked code snippets inline, for
        instance:
      
        * `f x Nat.+ sqr 1` - the `2` says to ignore the first
          two arguments when rendering. In richer renderers, the
          `sqr` link will be clickable.
        * If your snippet expression is just a single function
          application, you can put it in double backticks, like
          so: `sqr x`. This is equivalent to `sqr x`.

    # Non-Unison code blocks
    
      Use three or more single quotes to start a block with no
      syntax highlighting:
    
      ``` raw
         _____     _             
        |  |  |___|_|___ ___ ___ 
        |  |  |   | |_ -| . |   |
        |_____|_|_|_|___|___|_|_|
        
      ```
    
      You can use three or more backticks plus a language name
      for blocks with syntax highlighting:
    
      ``` Haskell
      -- A fenced code block which isn't parsed by Unison
      reverse = foldl (flip (:)) []
      ```
    
      ``` Scala
      // A fenced code block which isn't parsed by Unison
      def reverse[A](xs: List[A]) = 
        xs.foldLeft(Nil : List[A])((acc,a) => a +: acc)
      ```

    There are also asides, callouts, tables, tooltips, and more.
    These don't currently have special syntax; just use the
    `{{ }}` syntax to call these functions directly.
    
        docAside : Doc2 -> Doc2
    
        docCallout : Optional Doc2 -> Doc2 -> Doc2
    
        docBlockquote : Doc2 -> Doc2
    
        docTooltip : Doc2 -> Doc2 -> Doc2
    
        docTable : [[Doc2]] -> Doc2
    
    This is an aside. (
    Some extra detail that doesn't belong in main text. )
    
      | This is an important callout, with no icon.
    
      | 🌻
      | 
      | This is an important callout, with an icon. The text
      | wraps onto multiple lines.
    
    > "And what is the use of a book," thought Alice, "without
    > pictures or conversation?"
    > 
    > *Lewis Carroll, Alice's Adventures in Wonderland*
    
    Hover over me
    
    a           b           A longer paragraph that will split
                            onto multiple lines, such that this
                            row occupies multiple lines in the
                            rendered table.
    Some text   More text   Zounds!
````

But we can't display this due to a decompilation problem.

``` unison
rendered = Pretty.get (docFormatConsole doc.guide)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      rendered : Annotated () (Either SpecialForm ConsoleText)
```

```` ucm
scratch/main> display rendered

  # Unison computable documentation

    # Basic formatting
    
      Paragraphs are separated by one or more blanklines.
      Sections have a title and 0 or more paragraphs or other
      section elements.
    
      Text can be bold, *italicized*, ~~strikethrough~~, or
      `monospaced` (or `monospaced`).
    
      You can link to Unison terms, types, and external URLs:
    
      * An external url
      * Some is a term link; Optional is a type link
      * A named type link and a named term link. Term links are
        handy for linking to other documents!
    
      You can use `{{ .. }}` to escape out to regular Unison
      syntax, for instance __not bold__. This is useful for
      creating documents programmatically or just including
      other documents.
    
      *Next up:* lists

    # Lists
    
      # Bulleted lists
      
        Bulleted lists can use `+`, `-`, or `*` for the bullets
        (though the choice will be normalized away by the
        pretty-printer). They can be nested, to any depth:
      
        * A
        * B
        * C
          * C1
          * C2
    
      # Numbered lists
      
        1. A
        2. B
        3. C
      
        The first number of the list determines the starting
        number in the rendered output. The other numbers are
        ignored:
      
        10. A
        11. B
        12. C
      
        Numbered lists can be nested as well, and combined with
        bulleted lists:
      
        1. Wake up.
           * What am I doing here?
           * In this nested list.
        2. Take shower.
        3. Get dressed.

    # Evaluation
    
      Expressions can be evaluated inline, for instance `2`.
    
      Blocks of code can be evaluated as well, for instance:
    
          id x = x
          id (sqr 10)
          ⧨
          100
    
      also:
    
          match 1 with
            1 -> "hi"
            _ -> "goodbye"
          ⧨
          "hi"
    
      To include a typechecked snippet of code without
      evaluating it, you can do:
    
          use Nat *
          cube : Nat -> Nat
          cube x = x * x * x

    # Including Unison source code
    
      Unison definitions can be included in docs. For instance:
    
          structural type Optional a = Some a | None
          
          sqr : Nat -> Nat
          sqr x =
            use Nat *
            x * x
    
      Some rendering targets also support folded source:
    
          structural type Optional a = Some a | None
          
          sqr : Nat -> Nat
          sqr x =
            use Nat *
            x * x
    
      You can also include just a signature, inline, with
      `sqr : Nat -> Nat`, or you can include one or more
      signatures as a block:
    
          sqr : Nat -> Nat
      
          Nat.+ : Nat -> Nat -> Nat
    
      Or alternately:
    
          List.map : (a ->{e} b) -> [a] ->{e} [b]
    
      # Inline snippets
      
        You can include typechecked code snippets inline, for
        instance:
      
        * `f x Nat.+ sqr 1` - the `2` says to ignore the first
          two arguments when rendering. In richer renderers, the
          `sqr` link will be clickable.
        * If your snippet expression is just a single function
          application, you can put it in double backticks, like
          so: `sqr x`. This is equivalent to `sqr x`.

    # Non-Unison code blocks
    
      Use three or more single quotes to start a block with no
      syntax highlighting:
    
      ``` raw
         _____     _             
        |  |  |___|_|___ ___ ___ 
        |  |  |   | |_ -| . |   |
        |_____|_|_|_|___|___|_|_|
        
      ```
    
      You can use three or more backticks plus a language name
      for blocks with syntax highlighting:
    
      ``` Haskell
      -- A fenced code block which isn't parsed by Unison
      reverse = foldl (flip (:)) []
      ```
    
      ``` Scala
      // A fenced code block which isn't parsed by Unison
      def reverse[A](xs: List[A]) = 
        xs.foldLeft(Nil : List[A])((acc,a) => a +: acc)
      ```

    There are also asides, callouts, tables, tooltips, and more.
    These don't currently have special syntax; just use the
    `{{ }}` syntax to call these functions directly.
    
        docAside : Doc2 -> Doc2
    
        docCallout : Optional Doc2 -> Doc2 -> Doc2
    
        docBlockquote : Doc2 -> Doc2
    
        docTooltip : Doc2 -> Doc2 -> Doc2
    
        docTable : [[Doc2]] -> Doc2
    
    This is an aside. (
    Some extra detail that doesn't belong in main text. )
    
      | This is an important callout, with no icon.
    
      | 🌻
      | 
      | This is an important callout, with an icon. The text
      | wraps onto multiple lines.
    
    > "And what is the use of a book," thought Alice, "without
    > pictures or conversation?"
    > 
    > *Lewis Carroll, Alice's Adventures in Wonderland*
    
    Hover over me
    
    a           b           A longer paragraph that will split
                            onto multiple lines, such that this
                            row occupies multiple lines in the
                            rendered table.
    Some text   More text   Zounds!

scratch/main> add

  ⍟ I've added these definitions:

    rendered : Annotated () (Either SpecialForm ConsoleText)

scratch/main> display rendered

  # Unison computable documentation

    # Basic formatting
    
      Paragraphs are separated by one or more blanklines.
      Sections have a title and 0 or more paragraphs or other
      section elements.
    
      Text can be bold, *italicized*, ~~strikethrough~~, or
      `monospaced` (or `monospaced`).
    
      You can link to Unison terms, types, and external URLs:
    
      * An external url
      * Some is a term link; Optional is a type link
      * A named type link and a named term link. Term links are
        handy for linking to other documents!
    
      You can use `{{ .. }}` to escape out to regular Unison
      syntax, for instance __not bold__. This is useful for
      creating documents programmatically or just including
      other documents.
    
      *Next up:* lists

    # Lists
    
      # Bulleted lists
      
        Bulleted lists can use `+`, `-`, or `*` for the bullets
        (though the choice will be normalized away by the
        pretty-printer). They can be nested, to any depth:
      
        * A
        * B
        * C
          * C1
          * C2
    
      # Numbered lists
      
        1. A
        2. B
        3. C
      
        The first number of the list determines the starting
        number in the rendered output. The other numbers are
        ignored:
      
        10. A
        11. B
        12. C
      
        Numbered lists can be nested as well, and combined with
        bulleted lists:
      
        1. Wake up.
           * What am I doing here?
           * In this nested list.
        2. Take shower.
        3. Get dressed.

    # Evaluation
    
      Expressions can be evaluated inline, for instance `2`.
    
      Blocks of code can be evaluated as well, for instance:
    
          id x = x
          id (sqr 10)
          ⧨
          100
    
      also:
    
          match 1 with
            1 -> "hi"
            _ -> "goodbye"
          ⧨
          "hi"
    
      To include a typechecked snippet of code without
      evaluating it, you can do:
    
          use Nat *
          cube : Nat -> Nat
          cube x = x * x * x

    # Including Unison source code
    
      Unison definitions can be included in docs. For instance:
    
          structural type Optional a = Some a | None
          
          sqr : Nat -> Nat
          sqr x =
            use Nat *
            x * x
    
      Some rendering targets also support folded source:
    
          structural type Optional a = Some a | None
          
          sqr : Nat -> Nat
          sqr x =
            use Nat *
            x * x
    
      You can also include just a signature, inline, with
      `sqr : Nat -> Nat`, or you can include one or more
      signatures as a block:
    
          sqr : Nat -> Nat
      
          Nat.+ : Nat -> Nat -> Nat
    
      Or alternately:
    
          List.map : (a ->{e} b) -> [a] ->{e} [b]
    
      # Inline snippets
      
        You can include typechecked code snippets inline, for
        instance:
      
        * `f x Nat.+ sqr 1` - the `2` says to ignore the first
          two arguments when rendering. In richer renderers, the
          `sqr` link will be clickable.
        * If your snippet expression is just a single function
          application, you can put it in double backticks, like
          so: `sqr x`. This is equivalent to `sqr x`.

    # Non-Unison code blocks
    
      Use three or more single quotes to start a block with no
      syntax highlighting:
    
      ``` raw
         _____     _             
        |  |  |___|_|___ ___ ___ 
        |  |  |   | |_ -| . |   |
        |_____|_|_|_|___|___|_|_|
        
      ```
    
      You can use three or more backticks plus a language name
      for blocks with syntax highlighting:
    
      ``` Haskell
      -- A fenced code block which isn't parsed by Unison
      reverse = foldl (flip (:)) []
      ```
    
      ``` Scala
      // A fenced code block which isn't parsed by Unison
      def reverse[A](xs: List[A]) = 
        xs.foldLeft(Nil : List[A])((acc,a) => a +: acc)
      ```

    There are also asides, callouts, tables, tooltips, and more.
    These don't currently have special syntax; just use the
    `{{ }}` syntax to call these functions directly.
    
        docAside : Doc2 -> Doc2
    
        docCallout : Optional Doc2 -> Doc2 -> Doc2
    
        docBlockquote : Doc2 -> Doc2
    
        docTooltip : Doc2 -> Doc2 -> Doc2
    
        docTable : [[Doc2]] -> Doc2
    
    This is an aside. (
    Some extra detail that doesn't belong in main text. )
    
      | This is an important callout, with no icon.
    
      | 🌻
      | 
      | This is an important callout, with an icon. The text
      | wraps onto multiple lines.
    
    > "And what is the use of a book," thought Alice, "without
    > pictures or conversation?"
    > 
    > *Lewis Carroll, Alice's Adventures in Wonderland*
    
    Hover over me
    
    a           b           A longer paragraph that will split
                            onto multiple lines, such that this
                            row occupies multiple lines in the
                            rendered table.
    Some text   More text   Zounds!

scratch/main> undo

  Here are the changes I undid

  Added definitions:

    1. rendered : Annotated () (Either SpecialForm ConsoleText)
````

And then this sometimes generates a GHC crash "strange closure error" but doesn't seem deterministic.

``` unison
rendered = Pretty.get (docFormatConsole doc.guide)

> rendered
```

```` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      rendered : Annotated () (Either SpecialForm ConsoleText)

  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    3 | > rendered
          ⧩
          Annotated.Group
            ()
            (Annotated.Append
              ()
              [ Indent
                  ()
                  (Lit () (Right (Plain "# ")))
                  (Lit () (Right (Plain "  ")))
                  (Annotated.Group
                    ()
                    (Wrap
                      ()
                      (Annotated.Append
                        ()
                        [ Lit
                            ()
                            (Right
                              (ConsoleText.Bold (Plain "Unison")))
                        , Lit
                            ()
                            (Right
                              (ConsoleText.Bold
                                (Plain "computable")))
                        , Lit
                            ()
                            (Right
                              (ConsoleText.Bold
                                (Plain "documentation")))
                        ])))
              , Lit () (Right (Plain "\n"))
              , Lit () (Right (Plain "\n"))
              , Indent
                  ()
                  (Lit () (Right (Plain "  ")))
                  (Lit () (Right (Plain "  ")))
                  (Annotated.Group
                    ()
                    (Wrap
                      ()
                      (Annotated.Group
                        ()
                        (Annotated.Append
                          ()
                          [ Indent
                              ()
                              (Lit () (Right (Plain "# ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Lit
                                        ()
                                        (Right
                                          (ConsoleText.Bold
                                            (Plain "Basic")))
                                    , Lit
                                        ()
                                        (Right
                                          (ConsoleText.Bold
                                            (Plain "formatting")))
                                    ])))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Lit () (Right (Plain "  ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Lit
                                        ()
                                        (Right
                                          (Plain "Paragraphs"))
                                    , Lit
                                        () (Right (Plain "are"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain "separated"))
                                    , Lit
                                        () (Right (Plain "by"))
                                    , Lit
                                        () (Right (Plain "one"))
                                    , Lit
                                        () (Right (Plain "or"))
                                    , Lit
                                        ()
                                        (Right (Plain "more"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain "blanklines."))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain "Sections"))
                                    , Lit
                                        ()
                                        (Right (Plain "have"))
                                    , Lit () (Right (Plain "a"))
                                    , Lit
                                        ()
                                        (Right (Plain "title"))
                                    , Lit
                                        () (Right (Plain "and"))
                                    , Lit () (Right (Plain "0"))
                                    , Lit
                                        () (Right (Plain "or"))
                                    , Lit
                                        ()
                                        (Right (Plain "more"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain "paragraphs"))
                                    , Lit
                                        () (Right (Plain "or"))
                                    , Lit
                                        ()
                                        (Right (Plain "other"))
                                    , Lit
                                        ()
                                        (Right (Plain "section"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain "elements."))
                                    ])))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Lit () (Right (Plain "  ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Lit
                                        ()
                                        (Right (Plain "Text"))
                                    , Lit
                                        () (Right (Plain "can"))
                                    , Lit
                                        () (Right (Plain "be"))
                                    , Annotated.Group
                                        ()
                                        (Annotated.Append
                                          ()
                                          [ Wrap
                                              ()
                                              (Lit
                                                ()
                                                (Right
                                                  (ConsoleText.Bold
                                                    (Plain
                                                      "bold"))))
                                          , Lit
                                              ()
                                              (Right (Plain ","))
                                          ])
                                    , Annotated.Group
                                        ()
                                        (Annotated.Append
                                          ()
                                          [ Annotated.Group
                                              ()
                                              (Annotated.Append
                                                ()
                                                [ Lit
                                                    ()
                                                    (Right
                                                      (Plain "*"))
                                                , Wrap
                                                    ()
                                                    (Lit
                                                      ()
                                                      (Right
                                                        (Plain
                                                          "italicized")))
                                                , Lit
                                                    ()
                                                    (Right
                                                      (Plain "*"))
                                                ])
                                          , Lit
                                              ()
                                              (Right (Plain ","))
                                          ])
                                    , Annotated.Group
                                        ()
                                        (Annotated.Append
                                          ()
                                          [ Annotated.Group
                                              ()
                                              (Annotated.Append
                                                ()
                                                [ Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "~~"))
                                                , Wrap
                                                    ()
                                                    (Lit
                                                      ()
                                                      (Right
                                                        (Plain
                                                          "strikethrough")))
                                                , Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "~~"))
                                                ])
                                          , Lit
                                              ()
                                              (Right (Plain ","))
                                          ])
                                    , Lit
                                        () (Right (Plain "or"))
                                    , Annotated.Group
                                        ()
                                        (Annotated.Append
                                          ()
                                          [ Lit
                                              ()
                                              (Right (Plain "`"))
                                          , Lit
                                              ()
                                              (Right
                                                (Plain
                                                  "monospaced"))
                                          , Lit
                                              ()
                                              (Right (Plain "`"))
                                          ])
                                    , Lit
                                        () (Right (Plain "(or"))
                                    , Annotated.Group
                                        ()
                                        (Annotated.Append
                                          ()
                                          [ Annotated.Group
                                              ()
                                              (Annotated.Append
                                                ()
                                                [ Lit
                                                    ()
                                                    (Right
                                                      (Plain "`"))
                                                , Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "monospaced"))
                                                , Lit
                                                    ()
                                                    (Right
                                                      (Plain "`"))
                                                ])
                                          , Lit
                                              ()
                                              (Right
                                                (Plain ")."))
                                          ])
                                    ])))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Lit () (Right (Plain "  ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Lit
                                        () (Right (Plain "You"))
                                    , Lit
                                        () (Right (Plain "can"))
                                    , Lit
                                        ()
                                        (Right (Plain "link"))
                                    , Lit
                                        () (Right (Plain "to"))
                                    , Lit
                                        ()
                                        (Right (Plain "Unison"))
                                    , Lit
                                        ()
                                        (Right (Plain "terms,"))
                                    , Lit
                                        ()
                                        (Right (Plain "types,"))
                                    , Lit
                                        () (Right (Plain "and"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain "external"))
                                    , Lit
                                        ()
                                        (Right (Plain "URLs:"))
                                    ])))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Lit () (Right (Plain "  ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Annotated.Group
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Indent
                                        ()
                                        (Lit
                                          ()
                                          (Right (Plain "* ")))
                                        (Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Wrap
                                          ()
                                          (Wrap
                                            ()
                                            (Annotated.Append
                                              ()
                                              [ Lit
                                                  ()
                                                  (Right
                                                    (Underline
                                                      (Plain
                                                        "An")))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Underline
                                                      (Plain
                                                        "external")))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Underline
                                                      (Plain
                                                        "url")))
                                              ])))
                                    , Lit
                                        () (Right (Plain "\n"))
                                    , Indent
                                        ()
                                        (Lit
                                          ()
                                          (Right (Plain "* ")))
                                        (Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Wrap
                                          ()
                                          (Annotated.Append
                                            ()
                                            [ Lit
                                                ()
                                                (Left
                                                  (SpecialForm.Link
                                                    (Right
                                                      (Term.Term
                                                        (Any
                                                          (do
                                                            Some))))))
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain "is"))
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain "a"))
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain "term"))
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain "link;"))
                                            , Lit
                                                ()
                                                (Left
                                                  (SpecialForm.Link
                                                    (Left
                                                      (typeLink Optional))))
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain "is"))
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain "a"))
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain "type"))
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain "link"))
                                            ]))
                                    , Lit
                                        () (Right (Plain "\n"))
                                    , Indent
                                        ()
                                        (Lit
                                          ()
                                          (Right (Plain "* ")))
                                        (Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Wrap
                                          ()
                                          (Annotated.Append
                                            ()
                                            [ Wrap
                                                ()
                                                (Annotated.Append
                                                  ()
                                                  [ Lit
                                                      ()
                                                      (Right
                                                        (Underline
                                                          (Plain
                                                            "A")))
                                                  , Lit
                                                      ()
                                                      (Right
                                                        (Underline
                                                          (Plain
                                                            "named")))
                                                  , Lit
                                                      ()
                                                      (Right
                                                        (Underline
                                                          (Plain
                                                            "type")))
                                                  , Lit
                                                      ()
                                                      (Right
                                                        (Underline
                                                          (Plain
                                                            "link")))
                                                  ])
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain "and"))
                                            , Annotated.Group
                                                ()
                                                (Annotated.Append
                                                  ()
                                                  [ Wrap
                                                      ()
                                                      (Annotated.Append
                                                        ()
                                                        [ Lit
                                                            ()
                                                            (Right
                                                              (Underline
                                                                (Plain
                                                                  "a")))
                                                        , Lit
                                                            ()
                                                            (Right
                                                              (Underline
                                                                (Plain
                                                                  "named")))
                                                        , Lit
                                                            ()
                                                            (Right
                                                              (Underline
                                                                (Plain
                                                                  "term")))
                                                        , Lit
                                                            ()
                                                            (Right
                                                              (Underline
                                                                (Plain
                                                                  "link")))
                                                        ])
                                                  , Lit
                                                      ()
                                                      (Right
                                                        (Plain
                                                          "."))
                                                  ])
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain "Term"))
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain "links"))
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain "are"))
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain "handy"))
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain "for"))
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain
                                                    "linking"))
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain "to"))
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain "other"))
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain
                                                    "documents!"))
                                            ]))
                                    ])))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Lit () (Right (Plain "  ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Lit
                                        () (Right (Plain "You"))
                                    , Lit
                                        () (Right (Plain "can"))
                                    , Lit
                                        () (Right (Plain "use"))
                                    , Annotated.Group
                                        ()
                                        (Annotated.Append
                                          ()
                                          [ Lit
                                              ()
                                              (Right (Plain "`"))
                                          , Lit
                                              ()
                                              (Right
                                                (Plain
                                                  "{{ .. }}"))
                                          , Lit
                                              ()
                                              (Right (Plain "`"))
                                          ])
                                    , Lit
                                        () (Right (Plain "to"))
                                    , Lit
                                        ()
                                        (Right (Plain "escape"))
                                    , Lit
                                        () (Right (Plain "out"))
                                    , Lit
                                        () (Right (Plain "to"))
                                    , Lit
                                        ()
                                        (Right (Plain "regular"))
                                    , Lit
                                        ()
                                        (Right (Plain "Unison"))
                                    , Lit
                                        ()
                                        (Right (Plain "syntax,"))
                                    , Lit
                                        () (Right (Plain "for"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain "instance"))
                                    , Annotated.Group
                                        ()
                                        (Annotated.Append
                                          ()
                                          [ Lit
                                              ()
                                              (Right
                                                (Plain
                                                  "__not bold__"))
                                          , Lit
                                              ()
                                              (Right (Plain "."))
                                          ])
                                    , Lit
                                        ()
                                        (Right (Plain "This"))
                                    , Lit
                                        () (Right (Plain "is"))
                                    , Lit
                                        ()
                                        (Right (Plain "useful"))
                                    , Lit
                                        () (Right (Plain "for"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain "creating"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain "documents"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain
                                            "programmatically"))
                                    , Lit
                                        () (Right (Plain "or"))
                                    , Lit
                                        ()
                                        (Right (Plain "just"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain "including"))
                                    , Lit
                                        ()
                                        (Right (Plain "other"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain "documents."))
                                    ])))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Lit () (Right (Plain "  ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Annotated.Group
                                        ()
                                        (Annotated.Append
                                          ()
                                          [ Lit
                                              ()
                                              (Right (Plain "*"))
                                          , Lit
                                              ()
                                              (Right
                                                (Plain "Next"))
                                          ])
                                    , Annotated.Group
                                        ()
                                        (Annotated.Append
                                          ()
                                          [ Lit
                                              ()
                                              (Right
                                                (Plain "up:"))
                                          , Lit
                                              ()
                                              (Right (Plain "*"))
                                          ])
                                    , Lit
                                        ()
                                        (Left
                                          (SpecialForm.Link
                                            (Right
                                              (Term.Term
                                                (Any (do lists))))))
                                    ])))
                          ]))))
              , Lit () (Right (Plain "\n"))
              , Lit () (Right (Plain "\n"))
              , Indent
                  ()
                  (Lit () (Right (Plain "  ")))
                  (Lit () (Right (Plain "  ")))
                  (Annotated.Group
                    ()
                    (Wrap
                      ()
                      (Annotated.Group
                        ()
                        (Annotated.Append
                          ()
                          [ Indent
                              ()
                              (Lit () (Right (Plain "# ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Lit
                                    ()
                                    (Right
                                      (ConsoleText.Bold
                                        (Plain "Lists"))))))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Lit () (Right (Plain "  ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Annotated.Group
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Indent
                                        ()
                                        (Lit
                                          ()
                                          (Right (Plain "# ")))
                                        (Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Group
                                          ()
                                          (Wrap
                                            ()
                                            (Annotated.Append
                                              ()
                                              [ Lit
                                                  ()
                                                  (Right
                                                    (ConsoleText.Bold
                                                      (Plain
                                                        "Bulleted")))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (ConsoleText.Bold
                                                      (Plain
                                                        "lists")))
                                              ])))
                                    , Lit
                                        () (Right (Plain "\n"))
                                    , Lit
                                        () (Right (Plain "\n"))
                                    , Indent
                                        ()
                                        (Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Group
                                          ()
                                          (Wrap
                                            ()
                                            (Annotated.Append
                                              ()
                                              [ Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "Bulleted"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "lists"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "can"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "use"))
                                              , Annotated.Group
                                                  ()
                                                  (Annotated.Append
                                                    ()
                                                    [ Annotated.Group
                                                        ()
                                                        (Annotated.Append
                                                          ()
                                                          [ Lit
                                                              ()
                                                              (Right
                                                                (Plain
                                                                  "`"))
                                                          , Lit
                                                              ()
                                                              (Right
                                                                (Plain
                                                                  "+"))
                                                          , Lit
                                                              ()
                                                              (Right
                                                                (Plain
                                                                  "`"))
                                                          ])
                                                    , Lit
                                                        ()
                                                        (Right
                                                          (Plain
                                                            ","))
                                                    ])
                                              , Annotated.Group
                                                  ()
                                                  (Annotated.Append
                                                    ()
                                                    [ Annotated.Group
                                                        ()
                                                        (Annotated.Append
                                                          ()
                                                          [ Lit
                                                              ()
                                                              (Right
                                                                (Plain
                                                                  "`"))
                                                          , Lit
                                                              ()
                                                              (Right
                                                                (Plain
                                                                  "-"))
                                                          , Lit
                                                              ()
                                                              (Right
                                                                (Plain
                                                                  "`"))
                                                          ])
                                                    , Lit
                                                        ()
                                                        (Right
                                                          (Plain
                                                            ","))
                                                    ])
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "or"))
                                              , Annotated.Group
                                                  ()
                                                  (Annotated.Append
                                                    ()
                                                    [ Lit
                                                        ()
                                                        (Right
                                                          (Plain
                                                            "`"))
                                                    , Lit
                                                        ()
                                                        (Right
                                                          (Plain
                                                            "*"))
                                                    , Lit
                                                        ()
                                                        (Right
                                                          (Plain
                                                            "`"))
                                                    ])
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "for"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "the"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "bullets"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "(though"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "the"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "choice"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "will"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "be"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "normalized"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "away"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "by"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "the"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "pretty-printer)."))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "They"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "can"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "be"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "nested,"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "to"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "any"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "depth:"))
                                              ])))
                                    , Lit
                                        () (Right (Plain "\n"))
                                    , Lit
                                        () (Right (Plain "\n"))
                                    , Indent
                                        ()
                                        (Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Group
                                          ()
                                          (Annotated.Group
                                            ()
                                            (Annotated.Append
                                              ()
                                              [ Indent
                                                  ()
                                                  (Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "* ")))
                                                  (Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "  ")))
                                                  (Wrap
                                                    ()
                                                    (Lit
                                                      ()
                                                      (Right
                                                        (Plain
                                                          "A"))))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "\n"))
                                              , Indent
                                                  ()
                                                  (Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "* ")))
                                                  (Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "  ")))
                                                  (Wrap
                                                    ()
                                                    (Lit
                                                      ()
                                                      (Right
                                                        (Plain
                                                          "B"))))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "\n"))
                                              , Indent
                                                  ()
                                                  (Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "* ")))
                                                  (Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "  ")))
                                                  (Annotated.Append
                                                    ()
                                                    [ Wrap
                                                        ()
                                                        (Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "C")))
                                                    , Lit
                                                        ()
                                                        (Right
                                                          (Plain
                                                            "\n"))
                                                    , Annotated.Group
                                                        ()
                                                        (Annotated.Append
                                                          ()
                                                          [ Indent
                                                              ()
                                                              (Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "* ")))
                                                              (Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "  ")))
                                                              (Wrap
                                                                ( 
                                                                )
                                                                (Lit
                                                                  ( 
                                                                  )
                                                                  (Right
                                                                    (Plain
                                                                      "C1"))))
                                                          , Lit
                                                              ()
                                                              (Right
                                                                (Plain
                                                                  "\n"))
                                                          , Indent
                                                              ()
                                                              (Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "* ")))
                                                              (Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "  ")))
                                                              (Wrap
                                                                ( 
                                                                )
                                                                (Lit
                                                                  ( 
                                                                  )
                                                                  (Right
                                                                    (Plain
                                                                      "C2"))))
                                                          ])
                                                    ])
                                              ])))
                                    ])))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Lit () (Right (Plain "  ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Annotated.Group
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Indent
                                        ()
                                        (Lit
                                          ()
                                          (Right (Plain "# ")))
                                        (Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Group
                                          ()
                                          (Wrap
                                            ()
                                            (Annotated.Append
                                              ()
                                              [ Lit
                                                  ()
                                                  (Right
                                                    (ConsoleText.Bold
                                                      (Plain
                                                        "Numbered")))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (ConsoleText.Bold
                                                      (Plain
                                                        "lists")))
                                              ])))
                                    , Lit
                                        () (Right (Plain "\n"))
                                    , Lit
                                        () (Right (Plain "\n"))
                                    , Indent
                                        ()
                                        (Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Group
                                          ()
                                          (Annotated.Group
                                            ()
                                            (Annotated.Append
                                              ()
                                              [ Indent
                                                  ()
                                                  (Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "1. ")))
                                                  (Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "   ")))
                                                  (Wrap
                                                    ()
                                                    (Lit
                                                      ()
                                                      (Right
                                                        (Plain
                                                          "A"))))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "\n"))
                                              , Indent
                                                  ()
                                                  (Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "2. ")))
                                                  (Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "   ")))
                                                  (Wrap
                                                    ()
                                                    (Lit
                                                      ()
                                                      (Right
                                                        (Plain
                                                          "B"))))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "\n"))
                                              , Indent
                                                  ()
                                                  (Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "3. ")))
                                                  (Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "   ")))
                                                  (Wrap
                                                    ()
                                                    (Lit
                                                      ()
                                                      (Right
                                                        (Plain
                                                          "C"))))
                                              ])))
                                    , Lit
                                        () (Right (Plain "\n"))
                                    , Lit
                                        () (Right (Plain "\n"))
                                    , Indent
                                        ()
                                        (Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Group
                                          ()
                                          (Wrap
                                            ()
                                            (Annotated.Append
                                              ()
                                              [ Lit
                                                  ()
                                                  (Right
                                                    (Plain "The"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "first"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "number"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "of"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "the"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "list"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "determines"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "the"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "starting"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "number"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "in"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "the"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "rendered"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "output."))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "The"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "other"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "numbers"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "are"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "ignored:"))
                                              ])))
                                    , Lit
                                        () (Right (Plain "\n"))
                                    , Lit
                                        () (Right (Plain "\n"))
                                    , Indent
                                        ()
                                        (Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Group
                                          ()
                                          (Annotated.Group
                                            ()
                                            (Annotated.Append
                                              ()
                                              [ Indent
                                                  ()
                                                  (Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "10. ")))
                                                  (Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "    ")))
                                                  (Wrap
                                                    ()
                                                    (Lit
                                                      ()
                                                      (Right
                                                        (Plain
                                                          "A"))))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "\n"))
                                              , Indent
                                                  ()
                                                  (Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "11. ")))
                                                  (Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "    ")))
                                                  (Wrap
                                                    ()
                                                    (Lit
                                                      ()
                                                      (Right
                                                        (Plain
                                                          "B"))))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "\n"))
                                              , Indent
                                                  ()
                                                  (Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "12. ")))
                                                  (Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "    ")))
                                                  (Wrap
                                                    ()
                                                    (Lit
                                                      ()
                                                      (Right
                                                        (Plain
                                                          "C"))))
                                              ])))
                                    , Lit
                                        () (Right (Plain "\n"))
                                    , Lit
                                        () (Right (Plain "\n"))
                                    , Indent
                                        ()
                                        (Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Group
                                          ()
                                          (Wrap
                                            ()
                                            (Annotated.Append
                                              ()
                                              [ Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "Numbered"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "lists"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "can"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "be"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "nested"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "as"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "well,"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "and"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "combined"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "with"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "bulleted"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "lists:"))
                                              ])))
                                    , Lit
                                        () (Right (Plain "\n"))
                                    , Lit
                                        () (Right (Plain "\n"))
                                    , Indent
                                        ()
                                        (Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Group
                                          ()
                                          (Annotated.Group
                                            ()
                                            (Annotated.Append
                                              ()
                                              [ Indent
                                                  ()
                                                  (Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "1. ")))
                                                  (Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "   ")))
                                                  (Annotated.Append
                                                    ()
                                                    [ Wrap
                                                        ()
                                                        (Annotated.Append
                                                          ()
                                                          [ Lit
                                                              ()
                                                              (Right
                                                                (Plain
                                                                  "Wake"))
                                                          , Lit
                                                              ()
                                                              (Right
                                                                (Plain
                                                                  "up."))
                                                          ])
                                                    , Lit
                                                        ()
                                                        (Right
                                                          (Plain
                                                            "\n"))
                                                    , Annotated.Group
                                                        ()
                                                        (Annotated.Append
                                                          ()
                                                          [ Indent
                                                              ()
                                                              (Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "* ")))
                                                              (Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "  ")))
                                                              (Wrap
                                                                ( 
                                                                )
                                                                (Annotated.Append
                                                                  ( 
                                                                  )
                                                                  [ Lit
                                                                      ( 
                                                                      )
                                                                      (Right
                                                                        (Plain
                                                                          "What"))
                                                                  , Lit
                                                                      ( 
                                                                      )
                                                                      (Right
                                                                        (Plain
                                                                          "am"))
                                                                  , Lit
                                                                      ( 
                                                                      )
                                                                      (Right
                                                                        (Plain
                                                                          "I"))
                                                                  , Lit
                                                                      ( 
                                                                      )
                                                                      (Right
                                                                        (Plain
                                                                          "doing"))
                                                                  , Lit
                                                                      ( 
                                                                      )
                                                                      (Right
                                                                        (Plain
                                                                          "here?"))
                                                                  ]))
                                                          , Lit
                                                              ()
                                                              (Right
                                                                (Plain
                                                                  "\n"))
                                                          , Indent
                                                              ()
                                                              (Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "* ")))
                                                              (Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "  ")))
                                                              (Wrap
                                                                ( 
                                                                )
                                                                (Annotated.Append
                                                                  ( 
                                                                  )
                                                                  [ Lit
                                                                      ( 
                                                                      )
                                                                      (Right
                                                                        (Plain
                                                                          "In"))
                                                                  , Lit
                                                                      ( 
                                                                      )
                                                                      (Right
                                                                        (Plain
                                                                          "this"))
                                                                  , Lit
                                                                      ( 
                                                                      )
                                                                      (Right
                                                                        (Plain
                                                                          "nested"))
                                                                  , Lit
                                                                      ( 
                                                                      )
                                                                      (Right
                                                                        (Plain
                                                                          "list."))
                                                                  ]))
                                                          ])
                                                    ])
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "\n"))
                                              , Indent
                                                  ()
                                                  (Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "2. ")))
                                                  (Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "   ")))
                                                  (Wrap
                                                    ()
                                                    (Annotated.Append
                                                      ()
                                                      [ Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "Take"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "shower."))
                                                      ]))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "\n"))
                                              , Indent
                                                  ()
                                                  (Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "3. ")))
                                                  (Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "   ")))
                                                  (Wrap
                                                    ()
                                                    (Annotated.Append
                                                      ()
                                                      [ Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "Get"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "dressed."))
                                                      ]))
                                              ])))
                                    ])))
                          ]))))
              , Lit () (Right (Plain "\n"))
              , Lit () (Right (Plain "\n"))
              , Indent
                  ()
                  (Lit () (Right (Plain "  ")))
                  (Lit () (Right (Plain "  ")))
                  (Annotated.Group
                    ()
                    (Wrap
                      ()
                      (Annotated.Group
                        ()
                        (Annotated.Append
                          ()
                          [ Indent
                              ()
                              (Lit () (Right (Plain "# ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Lit
                                    ()
                                    (Right
                                      (ConsoleText.Bold
                                        (Plain "Evaluation"))))))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Lit () (Right (Plain "  ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Lit
                                        ()
                                        (Right
                                          (Plain "Expressions"))
                                    , Lit
                                        () (Right (Plain "can"))
                                    , Lit
                                        () (Right (Plain "be"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain "evaluated"))
                                    , Lit
                                        ()
                                        (Right (Plain "inline,"))
                                    , Lit
                                        () (Right (Plain "for"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain "instance"))
                                    , Annotated.Group
                                        ()
                                        (Annotated.Append
                                          ()
                                          [ Lit
                                              ()
                                              (Left
                                                (EvalInline
                                                  (Term.Term
                                                    (Any
                                                      (do
                                                        1
                                                          Nat.+ 1)))))
                                          , Lit
                                              ()
                                              (Right (Plain "."))
                                          ])
                                    ])))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Lit () (Right (Plain "  ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Lit
                                        ()
                                        (Right (Plain "Blocks"))
                                    , Lit
                                        () (Right (Plain "of"))
                                    , Lit
                                        ()
                                        (Right (Plain "code"))
                                    , Lit
                                        () (Right (Plain "can"))
                                    , Lit
                                        () (Right (Plain "be"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain "evaluated"))
                                    , Lit
                                        () (Right (Plain "as"))
                                    , Lit
                                        ()
                                        (Right (Plain "well,"))
                                    , Lit
                                        () (Right (Plain "for"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain "instance:"))
                                    ])))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Lit () (Right (Plain "  ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                () (Lit
                                  () (Left
                                    (Eval
                                      (Term.Term
                                        (Any
                                          (do
                                            id x = x
                                            id (sqr 10))))))))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Lit () (Right (Plain "  ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Lit
                                    () (Right (Plain "also:")))))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Lit () (Right (Plain "  ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                () (Lit
                                  () (Left
                                    (Eval
                                      (Term.Term
                                        (Any
                                          (do match 1 with
                                            1 -> "hi"
                                            _ -> "goodbye")))))))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Lit () (Right (Plain "  ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Lit
                                        () (Right (Plain "To"))
                                    , Lit
                                        ()
                                        (Right (Plain "include"))
                                    , Lit () (Right (Plain "a"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain "typechecked"))
                                    , Lit
                                        ()
                                        (Right (Plain "snippet"))
                                    , Lit
                                        () (Right (Plain "of"))
                                    , Lit
                                        ()
                                        (Right (Plain "code"))
                                    , Lit
                                        ()
                                        (Right (Plain "without"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain "evaluating"))
                                    , Lit
                                        () (Right (Plain "it,"))
                                    , Lit
                                        () (Right (Plain "you"))
                                    , Lit
                                        () (Right (Plain "can"))
                                    , Lit
                                        () (Right (Plain "do:"))
                                    ])))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Lit () (Right (Plain "  ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                () (Lit
                                  () (Left
                                    (ExampleBlock
                                      0 (Term.Term
                                        (Any
                                          (do
                                            use Nat *
                                            cube : Nat -> Nat
                                            cube x = x * x * x
                                            ())))))))
                          ]))))
              , Lit () (Right (Plain "\n"))
              , Lit () (Right (Plain "\n"))
              , Indent
                  ()
                  (Lit () (Right (Plain "  ")))
                  (Lit () (Right (Plain "  ")))
                  (Annotated.Group
                    ()
                    (Wrap
                      ()
                      (Annotated.Group
                        ()
                        (Annotated.Append
                          ()
                          [ Indent
                              ()
                              (Lit () (Right (Plain "# ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Lit
                                        ()
                                        (Right
                                          (ConsoleText.Bold
                                            (Plain "Including")))
                                    , Lit
                                        ()
                                        (Right
                                          (ConsoleText.Bold
                                            (Plain "Unison")))
                                    , Lit
                                        ()
                                        (Right
                                          (ConsoleText.Bold
                                            (Plain "source")))
                                    , Lit
                                        ()
                                        (Right
                                          (ConsoleText.Bold
                                            (Plain "code")))
                                    ])))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Lit () (Right (Plain "  ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Lit
                                        ()
                                        (Right (Plain "Unison"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain "definitions"))
                                    , Lit
                                        () (Right (Plain "can"))
                                    , Lit
                                        () (Right (Plain "be"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain "included"))
                                    , Lit
                                        () (Right (Plain "in"))
                                    , Lit
                                        ()
                                        (Right (Plain "docs."))
                                    , Lit
                                        () (Right (Plain "For"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain "instance:"))
                                    ])))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Lit () (Right (Plain "  ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Lit
                                    ()
                                    (Left
                                      (SpecialForm.Source
                                        [ ( Left
                                              (typeLink Optional)
                                          , []
                                          )
                                        , ( Right
                                              (Term.Term
                                                (Any (do sqr)))
                                          , []
                                          )
                                        ])))))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Lit () (Right (Plain "  ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Lit
                                        ()
                                        (Right (Plain "Some"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain "rendering"))
                                    , Lit
                                        ()
                                        (Right (Plain "targets"))
                                    , Lit
                                        ()
                                        (Right (Plain "also"))
                                    , Lit
                                        ()
                                        (Right (Plain "support"))
                                    , Lit
                                        ()
                                        (Right (Plain "folded"))
                                    , Lit
                                        ()
                                        (Right (Plain "source:"))
                                    ])))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Lit () (Right (Plain "  ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Lit
                                    ()
                                    (Left
                                      (FoldedSource
                                        [ ( Left
                                              (typeLink Optional)
                                          , []
                                          )
                                        , ( Right
                                              (Term.Term
                                                (Any (do sqr)))
                                          , []
                                          )
                                        ])))))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Lit () (Right (Plain "  ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Lit
                                        () (Right (Plain "You"))
                                    , Lit
                                        () (Right (Plain "can"))
                                    , Lit
                                        ()
                                        (Right (Plain "also"))
                                    , Lit
                                        ()
                                        (Right (Plain "include"))
                                    , Lit
                                        ()
                                        (Right (Plain "just"))
                                    , Lit () (Right (Plain "a"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain "signature,"))
                                    , Lit
                                        ()
                                        (Right (Plain "inline,"))
                                    , Lit
                                        ()
                                        (Right (Plain "with"))
                                    , Annotated.Group
                                        ()
                                        (Annotated.Append
                                          ()
                                          [ Lit
                                              ()
                                              (Left
                                                (SignatureInline
                                                  (Term.Term
                                                    (Any
                                                      (do sqr)))))
                                          , Lit
                                              ()
                                              (Right (Plain ","))
                                          ])
                                    , Lit
                                        () (Right (Plain "or"))
                                    , Lit
                                        () (Right (Plain "you"))
                                    , Lit
                                        () (Right (Plain "can"))
                                    , Lit
                                        ()
                                        (Right (Plain "include"))
                                    , Lit
                                        () (Right (Plain "one"))
                                    , Lit
                                        () (Right (Plain "or"))
                                    , Lit
                                        ()
                                        (Right (Plain "more"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain "signatures"))
                                    , Lit
                                        () (Right (Plain "as"))
                                    , Lit () (Right (Plain "a"))
                                    , Lit
                                        ()
                                        (Right (Plain "block:"))
                                    ])))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Lit () (Right (Plain "  ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Lit
                                    ()
                                    (Left
                                      (SpecialForm.Signature
                                        [ Term.Term
                                            (Any (do sqr))
                                        , Term.Term
                                            (Any (do (Nat.+)))
                                        ])))))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Lit () (Right (Plain "  ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Lit
                                        () (Right (Plain "Or"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain "alternately:"))
                                    ])))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Lit () (Right (Plain "  ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Lit
                                    ()
                                    (Left
                                      (SpecialForm.Signature
                                        [ Term.Term
                                            (Any (do List.map))
                                        ])))))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Lit () (Right (Plain "  ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Annotated.Group
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Indent
                                        ()
                                        (Lit
                                          ()
                                          (Right (Plain "# ")))
                                        (Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Group
                                          ()
                                          (Wrap
                                            ()
                                            (Annotated.Append
                                              ()
                                              [ Lit
                                                  ()
                                                  (Right
                                                    (ConsoleText.Bold
                                                      (Plain
                                                        "Inline")))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (ConsoleText.Bold
                                                      (Plain
                                                        "snippets")))
                                              ])))
                                    , Lit
                                        () (Right (Plain "\n"))
                                    , Lit
                                        () (Right (Plain "\n"))
                                    , Indent
                                        ()
                                        (Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Group
                                          ()
                                          (Wrap
                                            ()
                                            (Annotated.Append
                                              ()
                                              [ Lit
                                                  ()
                                                  (Right
                                                    (Plain "You"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "can"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "include"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "typechecked"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "code"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "snippets"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "inline,"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "for"))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "instance:"))
                                              ])))
                                    , Lit
                                        () (Right (Plain "\n"))
                                    , Lit
                                        () (Right (Plain "\n"))
                                    , Indent
                                        ()
                                        (Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Group
                                          ()
                                          (Annotated.Group
                                            ()
                                            (Annotated.Append
                                              ()
                                              [ Indent
                                                  ()
                                                  (Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "* ")))
                                                  (Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "  ")))
                                                  (Wrap
                                                    ()
                                                    (Annotated.Append
                                                      ()
                                                      [ Lit
                                                          ()
                                                          (Left
                                                            (Example
                                                              2
                                                              (Term.Term
                                                                (Any
                                                                  (do
                                                                    f
                                                                    x ->
                                                                      f
                                                                        x
                                                                        Nat.+ sqr
                                                                          1)))))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "-"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "the"))
                                                      , Annotated.Group
                                                          ()
                                                          (Annotated.Append
                                                            ()
                                                            [ Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "`"))
                                                            , Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "2"))
                                                            , Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "`"))
                                                            ])
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "says"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "to"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "ignore"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "the"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "first"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "two"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "arguments"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "when"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "rendering."))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "In"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "richer"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "renderers,"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "the"))
                                                      , Annotated.Group
                                                          ()
                                                          (Annotated.Append
                                                            ()
                                                            [ Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "`"))
                                                            , Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "sqr"))
                                                            , Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "`"))
                                                            ])
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "link"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "will"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "be"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "clickable."))
                                                      ]))
                                              , Lit
                                                  ()
                                                  (Right
                                                    (Plain "\n"))
                                              , Indent
                                                  ()
                                                  (Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "* ")))
                                                  (Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "  ")))
                                                  (Wrap
                                                    ()
                                                    (Annotated.Append
                                                      ()
                                                      [ Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "If"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "your"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "snippet"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "expression"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "is"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "just"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "a"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "single"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "function"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "application,"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "you"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "can"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "put"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "it"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "in"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "double"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "backticks,"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "like"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "so:"))
                                                      , Annotated.Group
                                                          ()
                                                          (Annotated.Append
                                                            ()
                                                            [ Lit
                                                                ( 
                                                                )
                                                                (Left
                                                                  (Example
                                                                    1
                                                                    (Term.Term
                                                                      (Any
                                                                        (do
                                                                          x ->
                                                                            sqr
                                                                              x)))))
                                                            , Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "."))
                                                            ])
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "This"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "is"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "equivalent"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "to"))
                                                      , Annotated.Group
                                                          ()
                                                          (Annotated.Append
                                                            ()
                                                            [ Lit
                                                                ( 
                                                                )
                                                                (Left
                                                                  (Example
                                                                    1
                                                                    (Term.Term
                                                                      (Any
                                                                        (do
                                                                          x ->
                                                                            sqr
                                                                              x)))))
                                                            , Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "."))
                                                            ])
                                                      ]))
                                              ])))
                                    ])))
                          ]))))
              , Lit () (Right (Plain "\n"))
              , Lit () (Right (Plain "\n"))
              , Indent
                  ()
                  (Lit () (Right (Plain "  ")))
                  (Lit () (Right (Plain "  ")))
                  (Annotated.Group
                    ()
                    (Wrap
                      ()
                      (Annotated.Group
                        ()
                        (Annotated.Append
                          ()
                          [ Indent
                              ()
                              (Lit () (Right (Plain "# ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Lit
                                        ()
                                        (Right
                                          (ConsoleText.Bold
                                            (Plain "Non-Unison")))
                                    , Lit
                                        ()
                                        (Right
                                          (ConsoleText.Bold
                                            (Plain "code")))
                                    , Lit
                                        ()
                                        (Right
                                          (ConsoleText.Bold
                                            (Plain "blocks")))
                                    ])))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Lit () (Right (Plain "  ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Lit
                                        () (Right (Plain "Use"))
                                    , Lit
                                        ()
                                        (Right (Plain "three"))
                                    , Lit
                                        () (Right (Plain "or"))
                                    , Lit
                                        ()
                                        (Right (Plain "more"))
                                    , Lit
                                        ()
                                        (Right (Plain "single"))
                                    , Lit
                                        ()
                                        (Right (Plain "quotes"))
                                    , Lit
                                        () (Right (Plain "to"))
                                    , Lit
                                        ()
                                        (Right (Plain "start"))
                                    , Lit () (Right (Plain "a"))
                                    , Lit
                                        ()
                                        (Right (Plain "block"))
                                    , Lit
                                        ()
                                        (Right (Plain "with"))
                                    , Lit
                                        () (Right (Plain "no"))
                                    , Lit
                                        ()
                                        (Right (Plain "syntax"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain "highlighting:"))
                                    ])))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Lit () (Right (Plain "  ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Group
                                    ()
                                    (Annotated.Append
                                      ()
                                      [ Lit
                                          ()
                                          (Right (Plain "``` "))
                                      , Annotated.Group
                                          ()
                                          (Lit
                                            ()
                                            (Right (Plain "raw")))
                                      , Lit
                                          ()
                                          (Right (Plain "\n"))
                                      , Lit
                                          ()
                                          (Right
                                            (Plain
                                              "   _____     _             \n  |  |  |___|_|___ ___ ___ \n  |  |  |   | |_ -| . |   |\n  |_____|_|_|_|___|___|_|_|\n  "))
                                      , Lit
                                          ()
                                          (Right (Plain "\n"))
                                      , Lit
                                          ()
                                          (Right (Plain "```"))
                                      ]))))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Lit () (Right (Plain "  ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Lit
                                        () (Right (Plain "You"))
                                    , Lit
                                        () (Right (Plain "can"))
                                    , Lit
                                        () (Right (Plain "use"))
                                    , Lit
                                        ()
                                        (Right (Plain "three"))
                                    , Lit
                                        () (Right (Plain "or"))
                                    , Lit
                                        ()
                                        (Right (Plain "more"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain "backticks"))
                                    , Lit
                                        ()
                                        (Right (Plain "plus"))
                                    , Lit () (Right (Plain "a"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain "language"))
                                    , Lit
                                        ()
                                        (Right (Plain "name"))
                                    , Lit
                                        () (Right (Plain "for"))
                                    , Lit
                                        ()
                                        (Right (Plain "blocks"))
                                    , Lit
                                        ()
                                        (Right (Plain "with"))
                                    , Lit
                                        ()
                                        (Right (Plain "syntax"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain "highlighting:"))
                                    ])))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Lit () (Right (Plain "  ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Annotated.Group
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Lit
                                        ()
                                        (Right (Plain "``` "))
                                    , Annotated.Group
                                        ()
                                        (Lit
                                          ()
                                          (Right
                                            (Plain "Haskell")))
                                    , Lit
                                        () (Right (Plain "\n"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain
                                            "-- A fenced code block which isn't parsed by Unison\nreverse = foldl (flip (:)) []"))
                                    , Lit
                                        () (Right (Plain "\n"))
                                    , Lit
                                        () (Right (Plain "```"))
                                    ])))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Lit () (Right (Plain "  ")))
                              (Lit () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Annotated.Group
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Lit
                                        ()
                                        (Right (Plain "``` "))
                                    , Annotated.Group
                                        ()
                                        (Lit
                                          ()
                                          (Right (Plain "Scala")))
                                    , Lit
                                        () (Right (Plain "\n"))
                                    , Lit
                                        ()
                                        (Right
                                          (Plain
                                            "// A fenced code block which isn't parsed by Unison\ndef reverse[A](xs: List[A]) = \n  xs.foldLeft(Nil : List[A])((acc,a) => a +: acc)"))
                                    , Lit
                                        () (Right (Plain "\n"))
                                    , Lit
                                        () (Right (Plain "```"))
                                    ])))
                          ]))))
              , Lit () (Right (Plain "\n"))
              , Lit () (Right (Plain "\n"))
              , Indent
                  ()
                  (Lit () (Right (Plain "  ")))
                  (Lit () (Right (Plain "  ")))
                  (Annotated.Group
                    ()
                    (Wrap
                      ()
                      (Annotated.Group
                        ()
                        (Annotated.Append
                          ()
                          [ Annotated.Group
                              ()
                              (Wrap
                                ()
                                (Annotated.Append
                                  ()
                                  [ Lit
                                      () (Right (Plain "There"))
                                  , Lit () (Right (Plain "are"))
                                  , Lit
                                      () (Right (Plain "also"))
                                  , Lit
                                      ()
                                      (Right (Plain "asides,"))
                                  , Lit
                                      ()
                                      (Right (Plain "callouts,"))
                                  , Lit
                                      ()
                                      (Right (Plain "tables,"))
                                  , Lit
                                      ()
                                      (Right (Plain "tooltips,"))
                                  , Lit () (Right (Plain "and"))
                                  , Lit
                                      () (Right (Plain "more."))
                                  , Lit
                                      () (Right (Plain "These"))
                                  , Lit
                                      () (Right (Plain "don't"))
                                  , Lit
                                      ()
                                      (Right (Plain "currently"))
                                  , Lit
                                      () (Right (Plain "have"))
                                  , Lit
                                      ()
                                      (Right (Plain "special"))
                                  , Lit
                                      ()
                                      (Right (Plain "syntax;"))
                                  , Lit
                                      () (Right (Plain "just"))
                                  , Lit () (Right (Plain "use"))
                                  , Lit () (Right (Plain "the"))
                                  , Annotated.Group
                                      ()
                                      (Annotated.Append
                                        ()
                                        [ Lit
                                            ()
                                            (Right (Plain "`"))
                                        , Lit
                                            ()
                                            (Right
                                              (Plain "{{ }}"))
                                        , Lit
                                            ()
                                            (Right (Plain "`"))
                                        ])
                                  , Lit
                                      ()
                                      (Right (Plain "syntax"))
                                  , Lit () (Right (Plain "to"))
                                  , Lit
                                      () (Right (Plain "call"))
                                  , Lit
                                      () (Right (Plain "these"))
                                  , Lit
                                      ()
                                      (Right (Plain "functions"))
                                  , Lit
                                      ()
                                      (Right (Plain "directly."))
                                  ]))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Annotated.Group
                              ()
                              (Wrap
                                ()
                                (Lit
                                  ()
                                  (Left
                                    (SpecialForm.Signature
                                      [ Term.Term
                                          (Any (do docAside))
                                      , Term.Term
                                          (Any (do docCallout))
                                      , Term.Term
                                          (Any
                                            (do docBlockquote))
                                      , Term.Term
                                          (Any (do docTooltip))
                                      , Term.Term
                                          (Any (do docTable))
                                      ]))))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Annotated.Group
                              ()
                              (Wrap
                                ()
                                (Annotated.Append
                                  ()
                                  [ Lit
                                      () (Right (Plain "This"))
                                  , Lit () (Right (Plain "is"))
                                  , Lit () (Right (Plain "an"))
                                  , Lit
                                      ()
                                      (Right (Plain "aside."))
                                  , Lit
                                      ()
                                      (Right
                                        (Foreground
                                          BrightBlack
                                          (Plain "(")))
                                  , Wrap
                                      ()
                                      (Annotated.Append
                                        ()
                                        [ Lit
                                            ()
                                            (Right
                                              (Foreground
                                                BrightBlack
                                                (Plain "Some")))
                                        , Lit
                                            ()
                                            (Right
                                              (Foreground
                                                BrightBlack
                                                (Plain "extra")))
                                        , Lit
                                            ()
                                            (Right
                                              (Foreground
                                                BrightBlack
                                                (Plain "detail")))
                                        , Lit
                                            ()
                                            (Right
                                              (Foreground
                                                BrightBlack
                                                (Plain "that")))
                                        , Lit
                                            ()
                                            (Right
                                              (Foreground
                                                BrightBlack
                                                (Plain "doesn't")))
                                        , Lit
                                            ()
                                            (Right
                                              (Foreground
                                                BrightBlack
                                                (Plain "belong")))
                                        , Lit
                                            ()
                                            (Right
                                              (Foreground
                                                BrightBlack
                                                (Plain "in")))
                                        , Lit
                                            ()
                                            (Right
                                              (Foreground
                                                BrightBlack
                                                (Plain "main")))
                                        , Lit
                                            ()
                                            (Right
                                              (Foreground
                                                BrightBlack
                                                (Plain "text.")))
                                        ])
                                  , Lit
                                      ()
                                      (Right
                                        (Foreground
                                          BrightBlack
                                          (Plain ")")))
                                  ]))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Annotated.Group
                              ()
                              (Wrap
                                ()
                                (Annotated.Group
                                  ()
                                  (Indent
                                    ()
                                    (Lit
                                      () (Right (Plain "  | ")))
                                    (Lit
                                      () (Right (Plain "  | ")))
                                    (Wrap
                                      ()
                                      (Annotated.Append
                                        ()
                                        [ Lit
                                            ()
                                            (Right
                                              (Plain "This"))
                                        , Lit
                                            ()
                                            (Right (Plain "is"))
                                        , Lit
                                            ()
                                            (Right (Plain "an"))
                                        , Lit
                                            ()
                                            (Right
                                              (Plain "important"))
                                        , Lit
                                            ()
                                            (Right
                                              (Plain "callout,"))
                                        , Lit
                                            ()
                                            (Right
                                              (Plain "with"))
                                        , Lit
                                            ()
                                            (Right (Plain "no"))
                                        , Lit
                                            ()
                                            (Right
                                              (Plain "icon."))
                                        ])))))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Annotated.Group
                              ()
                              (Wrap
                                ()
                                (Annotated.Group
                                  ()
                                  (Indent
                                    ()
                                    (Lit
                                      () (Right (Plain "  | ")))
                                    (Lit
                                      () (Right (Plain "  | ")))
                                    (Annotated.Append
                                      ()
                                      [ Wrap
                                          ()
                                          (Lit
                                            ()
                                            (Right
                                              (ConsoleText.Bold
                                                (Plain "🌻"))))
                                      , Lit
                                          ()
                                          (Right (Plain "\n"))
                                      , Lit
                                          () (Right (Plain ""))
                                      , Lit
                                          ()
                                          (Right (Plain "\n"))
                                      , Wrap
                                          ()
                                          (Annotated.Append
                                            ()
                                            [ Lit
                                                ()
                                                (Right
                                                  (Plain "This"))
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain "is"))
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain "an"))
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain
                                                    "important"))
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain
                                                    "callout,"))
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain "with"))
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain "an"))
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain "icon."))
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain "The"))
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain "text"))
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain "wraps"))
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain "onto"))
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain
                                                    "multiple"))
                                            , Lit
                                                ()
                                                (Right
                                                  (Plain
                                                    "lines."))
                                            ])
                                      ]))))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Annotated.Group
                              ()
                              (Wrap
                                ()
                                (Annotated.Group
                                  ()
                                  (Indent
                                    ()
                                    (Lit () (Right (Plain "> ")))
                                    (Lit () (Right (Plain "> ")))
                                    (Annotated.Group
                                      ()
                                      (Annotated.Append
                                        ()
                                        [ Annotated.Group
                                            ()
                                            (Wrap
                                              ()
                                              (Annotated.Append
                                                ()
                                                [ Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "\"And"))
                                                , Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "what"))
                                                , Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "is"))
                                                , Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "the"))
                                                , Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "use"))
                                                , Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "of"))
                                                , Lit
                                                    ()
                                                    (Right
                                                      (Plain "a"))
                                                , Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "book,\""))
                                                , Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "thought"))
                                                , Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "Alice,"))
                                                , Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "\"without"))
                                                , Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "pictures"))
                                                , Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "or"))
                                                , Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "conversation?\""))
                                                ]))
                                        , Lit
                                            ()
                                            (Right (Plain "\n"))
                                        , Lit
                                            ()
                                            (Right (Plain "\n"))
                                        , Annotated.Group
                                            ()
                                            (Wrap
                                              ()
                                              (Annotated.Append
                                                ()
                                                [ Annotated.Group
                                                    ()
                                                    (Annotated.Append
                                                      ()
                                                      [ Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "*"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "Lewis"))
                                                      ])
                                                , Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "Carroll,"))
                                                , Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "Alice's"))
                                                , Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "Adventures"))
                                                , Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "in"))
                                                , Annotated.Group
                                                    ()
                                                    (Annotated.Append
                                                      ()
                                                      [ Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "Wonderland"))
                                                      , Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "*"))
                                                      ])
                                                ]))
                                        ])))))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Annotated.Group
                              ()
                              (Wrap
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Lit
                                        ()
                                        (Right (Plain "Hover"))
                                    , Lit
                                        ()
                                        (Right (Plain "over"))
                                    , Lit
                                        () (Right (Plain "me"))
                                    ])))
                          , Lit () (Right (Plain "\n"))
                          , Lit () (Right (Plain "\n"))
                          , Annotated.Group
                              ()
                              (Wrap
                                ()
                                (Annotated.Table
                                  ()
                                  [ [ Wrap
                                        ()
                                        (Lit
                                          () (Right (Plain "a")))
                                    , Wrap
                                        ()
                                        (Lit
                                          () (Right (Plain "b")))
                                    , Wrap
                                        ()
                                        (Annotated.Append
                                          ()
                                          [ Lit
                                              ()
                                              (Right (Plain "A"))
                                          , Lit
                                              ()
                                              (Right
                                                (Plain "longer"))
                                          , Lit
                                              ()
                                              (Right
                                                (Plain
                                                  "paragraph"))
                                          , Lit
                                              ()
                                              (Right
                                                (Plain "that"))
                                          , Lit
                                              ()
                                              (Right
                                                (Plain "will"))
                                          , Lit
                                              ()
                                              (Right
                                                (Plain "split"))
                                          , Lit
                                              ()
                                              (Right
                                                (Plain "onto"))
                                          , Lit
                                              ()
                                              (Right
                                                (Plain
                                                  "multiple"))
                                          , Lit
                                              ()
                                              (Right
                                                (Plain "lines,"))
                                          , Lit
                                              ()
                                              (Right
                                                (Plain "such"))
                                          , Lit
                                              ()
                                              (Right
                                                (Plain "that"))
                                          , Lit
                                              ()
                                              (Right
                                                (Plain "this"))
                                          , Lit
                                              ()
                                              (Right
                                                (Plain "row"))
                                          , Lit
                                              ()
                                              (Right
                                                (Plain
                                                  "occupies"))
                                          , Lit
                                              ()
                                              (Right
                                                (Plain
                                                  "multiple"))
                                          , Lit
                                              ()
                                              (Right
                                                (Plain "lines"))
                                          , Lit
                                              ()
                                              (Right
                                                (Plain "in"))
                                          , Lit
                                              ()
                                              (Right
                                                (Plain "the"))
                                          , Lit
                                              ()
                                              (Right
                                                (Plain
                                                  "rendered"))
                                          , Lit
                                              ()
                                              (Right
                                                (Plain "table."))
                                          ])
                                    ]
                                  , [ Wrap
                                        ()
                                        (Annotated.Append
                                          ()
                                          [ Lit
                                              ()
                                              (Right
                                                (Plain "Some"))
                                          , Lit
                                              ()
                                              (Right
                                                (Plain "text"))
                                          ])
                                    , Wrap
                                        ()
                                        (Annotated.Append
                                          ()
                                          [ Lit
                                              ()
                                              (Right
                                                (Plain "More"))
                                          , Lit
                                              ()
                                              (Right
                                                (Plain "text"))
                                          ])
                                    , Wrap
                                        ()
                                        (Lit
                                          ()
                                          (Right
                                            (Plain "Zounds!")))
                                    ]
                                  ]))
                          ]))))
              ])
````

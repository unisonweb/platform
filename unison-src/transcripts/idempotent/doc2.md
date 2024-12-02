# Test parsing and round-trip of doc2 syntax elements

``` ucm :hide
scratch/main> builtins.mergeio
```

``` unison :hide
otherDoc : a -> Doc2
otherDoc _ = {{ yo }}

otherTerm : Nat
otherTerm = 99

fulldoc : Doc2
fulldoc =
  use Nat +
  {{
Heres some text with a
soft line break

hard line break

Here's a cool **BOLD** __italic__ ~~strikethrough~~ thing with an inline code block `1 + 2`

Should print with appropriate fences for the contents:

`No fancy quotes`

'' There are `backticks` in here ''

''' There are `backticks` and ''quotes'' in here '''

# Heading

## Heading 2

Term Link: {otherTerm}

Type Link: {type Optional}

Term source:

@source{term}

Term signature:

@signature{term}

* List item

Inline code:

`` 1 + 2 ``

` "doesn't typecheck" + 1 `

[Link](https://unison-lang.org)

![Image](https://share-next.unison-lang.org/static/unison-logo-circle.png)

Horizontal rule

---

Video

{{
Special
    (Embed
    (Any (Video [MediaSource "test.mp4" None] [("poster", "test.png")])))
}}

Transclusion/evaluation:

{{ otherDoc (a -> Word a) }}

---

The following markdown features aren't supported by the Doc format yet, but maybe will someday


> Block quote


Table

| Header 1 | Header 2 |
| -------- | -------- |
| Cell 1 | Cell 2 |


    Indented Code block

'''
    Exact whitespace should be preserved across multiple updates. Don't mess with the logo!

     _____     _
    |  |  |___|_|___ ___ ___
    |  |  |   | |_ -| . |   |
    |_____|_|_|_|___|___|_|_|

    Line with no whitespace:

    Should have one full trailing newline below here:

'''

Inline '' text literal with 1 space of padding '' in the middle of a sentence.


}}
```

Format it to check that everything pretty-prints in a valid way.

``` ucm
scratch/main> debug.format
```

``` unison :added-by-ucm scratch.u
otherDoc : a -> Doc2
otherDoc _ = {{ yo }}

otherTerm : Nat
otherTerm = 99

fulldoc : Doc2
fulldoc =
  use Nat +
  {{
  Heres some text with a soft line break
  
  hard line break
  
  Here's a cool **BOLD** __italic__ ~~strikethrough~~ thing with an inline code
  block `1 + 2`
  
  Should print with appropriate fences for the contents:
  
  `No fancy quotes`
  
  '' There are `backticks` in here ''
  
  ''' There are `backticks` and ''quotes'' in here '''
  
  # Heading
  
    ## Heading 2
    
       Term Link: {otherTerm}
       
       Type Link: {type Optional}
       
       Term source:
       
           @source{term}
       
       Term signature:
       
           @signature{term}
       
       * List item
       
       Inline code:
       
       `` 1 + 2 ``
       
       ` "doesn't typecheck" + 1 `
       
       [Link](https://unison-lang.org)
       
       ![Image](https://share-next.unison-lang.org/static/unison-logo-circle.png)
       
       Horizontal rule
       
       ---
       
       Video
       
       {{
       Special
         (Embed
           (Any (Video [MediaSource "test.mp4" None] [("poster", "test.png")])))
       }}
       
       Transclusion/evaluation:
       
       {{ otherDoc (a -> Word a) }}
       
       ---
       
       The following markdown features aren't supported by the Doc format yet,
       but maybe will someday
       
       > Block quote
       
       Table
       
       | Header 1 | Header 2 | | -------- | -------- | | Cell 1 | Cell 2 |
       
       Indented Code block
       
       '''
           Exact whitespace should be preserved across multiple updates. Don't mess with the logo!
       
            _____     _
           |  |  |___|_|___ ___ ___
           |  |  |   | |_ -| . |   |
           |_____|_|_|_|___|___|_|_|
       
           Line with no whitespace:
       
           Should have one full trailing newline below here:
       
       '''
       
       Inline ` text literal with 1 space of padding ` in the middle of a
       sentence.
  }}
```

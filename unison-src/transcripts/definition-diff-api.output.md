``` ucm
diffs/main> builtins.mergeio lib.builtins

  Done.

diffs/main> alias.term lib.builtins.Nat.gt lib.builtins.Nat.>

  Done.

diffs/main> alias.term lib.builtins.Nat.drop lib.builtins.Nat.-

  Done.

```
``` unison
term =
  _ = "Here's some text"
  1 + 1

type Type = Type Nat

ability Stream a where
  emit : a -> ()

take n s =
  use Nat > -
  h n = cases
    { emit a -> k } -> if n > 0
                         then
                           emit a
                           handle k() with h (n - 1)
                         else None
    { r }  -> Some r
  handle s() with h n

fakeRefModify f g = g []

foreach f xs = match xs with
    [] -> ()
    x +: rest -> let
      f x
      foreach f rest

handleRequest =
  use List +:
  finalizers = [1, 2, 3]
  addFinalizer f = fakeRefModify finalizers (fs -> f +: fs)
  foreach (f -> ()) finalizers

```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      ability Stream a
      type Type
      fakeRefModify : f -> ([elem] ->{g} t) ->{g} t
      foreach       : (i ->{g} ()) -> [i] ->{g} ()
      handleRequest : ()
      take          : Nat -> '{g} t ->{g, Stream a} Optional t
      term          : Nat

```
``` ucm
diffs/main> add

  ⍟ I've added these definitions:
  
    ability Stream a
    type Type
    fakeRefModify : f -> ([elem] ->{g} t) ->{g} t
    foreach       : (i ->{g} ()) -> [i] ->{g} ()
    handleRequest : ()
    take          : Nat -> '{g} t ->{g, Stream a} Optional t
    term          : Nat

diffs/main> branch.create new

  Done. I've created the new branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /new`.

```
``` unison
term =
  _ = "Here's some different text"
  1 + 2

type Type a = Type a Text

ability Stream a where
  emit : a -> ()

take n s =
  use Nat > -
  h n = cases
    { emit a -> k } ->
        emit a
        if n > 0
          then handle k() with h (n - 1)
          else None
    { r }  -> Some r
  if n > 0
    then handle s () with h (n - 1)
    else None

fakeRefModify2 f g = g []

foreach xs f = match xs with
    [] -> ()
    x +: rest -> let
      f x
      foreach rest f

handleRequest =
    use List +:
    finalizers = [1, 2, 3]
    addFinalizer f = fakeRefModify2 finalizers (fs -> (f +: fs, ()))
    foreach finalizers (f -> ())
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⊡ Previously added definitions will be ignored: Stream
    
    ⍟ These new definitions are ok to `add`:
    
      fakeRefModify2 : f -> ([elem] ->{g} t) ->{g} t
        (also named fakeRefModify)
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      type Type a
      foreach       : [t] -> (t ->{g} ()) ->{g} ()
      handleRequest : ()
      take          : Nat -> '{g} t ->{g, Stream a} Optional t
      term          : Nat

```
``` ucm
diffs/new> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

```
Diff terms

``` api
GET /api/projects/diffs/diff/terms?oldBranchRef=main&newBranchRef=new&oldTerm=term&newTerm=term
{
    "diff": {
        "contents": [
            {
                "diffTag": "both",
                "elements": [
                    {
                        "annotation": {
                            "contents": "term",
                            "tag": "HashQualifier"
                        },
                        "segment": "term"
                    },
                    {
                        "annotation": {
                            "tag": "TypeAscriptionColon"
                        },
                        "segment": " :"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "contents": "##Nat",
                            "tag": "TypeReference"
                        },
                        "segment": "Nat"
                    },
                    {
                        "annotation": null,
                        "segment": "\n"
                    },
                    {
                        "annotation": {
                            "contents": "term",
                            "tag": "HashQualifier"
                        },
                        "segment": "term"
                    },
                    {
                        "annotation": {
                            "tag": "BindingEquals"
                        },
                        "segment": " ="
                    },
                    {
                        "annotation": null,
                        "segment": "\n"
                    },
                    {
                        "annotation": null,
                        "segment": "  "
                    },
                    {
                        "annotation": {
                            "tag": "UseKeyword"
                        },
                        "segment": "use "
                    },
                    {
                        "annotation": {
                            "tag": "UsePrefix"
                        },
                        "segment": "Nat"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "UseSuffix"
                        },
                        "segment": "+"
                    },
                    {
                        "annotation": null,
                        "segment": "\n"
                    },
                    {
                        "annotation": null,
                        "segment": "  "
                    },
                    {
                        "annotation": {
                            "contents": "_",
                            "tag": "HashQualifier"
                        },
                        "segment": "_"
                    },
                    {
                        "annotation": {
                            "tag": "BindingEquals"
                        },
                        "segment": " ="
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    }
                ]
            },
            {
                "diffTag": "old",
                "elements": [
                    {
                        "annotation": {
                            "tag": "TextLiteral"
                        },
                        "segment": "\"Here's some text\""
                    }
                ]
            },
            {
                "diffTag": "new",
                "elements": [
                    {
                        "annotation": {
                            "tag": "TextLiteral"
                        },
                        "segment": "\"Here's some different text\""
                    }
                ]
            },
            {
                "diffTag": "both",
                "elements": [
                    {
                        "annotation": null,
                        "segment": "\n"
                    },
                    {
                        "annotation": null,
                        "segment": "  "
                    },
                    {
                        "annotation": {
                            "tag": "NumericLiteral"
                        },
                        "segment": "1"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "contents": "##Nat.+",
                            "tag": "TermReference"
                        },
                        "segment": "+"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    }
                ]
            },
            {
                "diffTag": "old",
                "elements": [
                    {
                        "annotation": {
                            "tag": "NumericLiteral"
                        },
                        "segment": "1"
                    }
                ]
            },
            {
                "diffTag": "new",
                "elements": [
                    {
                        "annotation": {
                            "tag": "NumericLiteral"
                        },
                        "segment": "2"
                    }
                ]
            }
        ],
        "tag": "UserObject"
    },
    "diffKind": "diff",
    "newBranchRef": "new",
    "newTerm": {
        "bestTermName": "term",
        "defnTermTag": "Plain",
        "signature": [
            {
                "annotation": {
                    "contents": "##Nat",
                    "tag": "TypeReference"
                },
                "segment": "Nat"
            }
        ],
        "termDefinition": {
            "contents": [
                {
                    "annotation": {
                        "contents": "term",
                        "tag": "HashQualifier"
                    },
                    "segment": "term"
                },
                {
                    "annotation": {
                        "tag": "TypeAscriptionColon"
                    },
                    "segment": " :"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "##Nat",
                        "tag": "TypeReference"
                    },
                    "segment": "Nat"
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": {
                        "contents": "term",
                        "tag": "HashQualifier"
                    },
                    "segment": "term"
                },
                {
                    "annotation": {
                        "tag": "BindingEquals"
                    },
                    "segment": " ="
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "tag": "UseKeyword"
                    },
                    "segment": "use "
                },
                {
                    "annotation": {
                        "tag": "UsePrefix"
                    },
                    "segment": "Nat"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "UseSuffix"
                    },
                    "segment": "+"
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "contents": "_",
                        "tag": "HashQualifier"
                    },
                    "segment": "_"
                },
                {
                    "annotation": {
                        "tag": "BindingEquals"
                    },
                    "segment": " ="
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "TextLiteral"
                    },
                    "segment": "\"Here's some different text\""
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "tag": "NumericLiteral"
                    },
                    "segment": "1"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "##Nat.+",
                        "tag": "TermReference"
                    },
                    "segment": "+"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "NumericLiteral"
                    },
                    "segment": "2"
                }
            ],
            "tag": "UserObject"
        },
        "termDocs": [],
        "termNames": [
            "term"
        ]
    },
    "oldBranchRef": "main",
    "oldTerm": {
        "bestTermName": "term",
        "defnTermTag": "Plain",
        "signature": [
            {
                "annotation": {
                    "contents": "##Nat",
                    "tag": "TypeReference"
                },
                "segment": "Nat"
            }
        ],
        "termDefinition": {
            "contents": [
                {
                    "annotation": {
                        "contents": "term",
                        "tag": "HashQualifier"
                    },
                    "segment": "term"
                },
                {
                    "annotation": {
                        "tag": "TypeAscriptionColon"
                    },
                    "segment": " :"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "##Nat",
                        "tag": "TypeReference"
                    },
                    "segment": "Nat"
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": {
                        "contents": "term",
                        "tag": "HashQualifier"
                    },
                    "segment": "term"
                },
                {
                    "annotation": {
                        "tag": "BindingEquals"
                    },
                    "segment": " ="
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "tag": "UseKeyword"
                    },
                    "segment": "use "
                },
                {
                    "annotation": {
                        "tag": "UsePrefix"
                    },
                    "segment": "Nat"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "UseSuffix"
                    },
                    "segment": "+"
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "contents": "_",
                        "tag": "HashQualifier"
                    },
                    "segment": "_"
                },
                {
                    "annotation": {
                        "tag": "BindingEquals"
                    },
                    "segment": " ="
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "TextLiteral"
                    },
                    "segment": "\"Here's some text\""
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "tag": "NumericLiteral"
                    },
                    "segment": "1"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "##Nat.+",
                        "tag": "TermReference"
                    },
                    "segment": "+"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "NumericLiteral"
                    },
                    "segment": "1"
                }
            ],
            "tag": "UserObject"
        },
        "termDocs": [],
        "termNames": [
            "term"
        ]
    },
    "project": "diffs"
}
```

More complex diff

``` api
GET /api/projects/diffs/diff/terms?oldBranchRef=main&newBranchRef=new&oldTerm=take&newTerm=take
{
    "diff": {
        "contents": [
            {
                "diffTag": "both",
                "elements": [
                    {
                        "annotation": {
                            "contents": "take",
                            "tag": "HashQualifier"
                        },
                        "segment": "take"
                    },
                    {
                        "annotation": {
                            "tag": "TypeAscriptionColon"
                        },
                        "segment": " :"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "contents": "##Nat",
                            "tag": "TypeReference"
                        },
                        "segment": "Nat"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "TypeOperator"
                        },
                        "segment": "->"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "DelayForceChar"
                        },
                        "segment": "'"
                    },
                    {
                        "annotation": {
                            "tag": "AbilityBraces"
                        },
                        "segment": "{"
                    },
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "g"
                    },
                    {
                        "annotation": {
                            "tag": "AbilityBraces"
                        },
                        "segment": "}"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "t"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "TypeOperator"
                        },
                        "segment": "->"
                    },
                    {
                        "annotation": {
                            "tag": "AbilityBraces"
                        },
                        "segment": "{"
                    },
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "g"
                    },
                    {
                        "annotation": null,
                        "segment": ","
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "contents": "#b035k0tpdv9jbs80ig29hujmv9kpkubda6or4320o5g7aj7edsudislnp2uovntgu5b0e6a18p0p7j8r2hcpr20blls7am8nll6t2ro",
                            "tag": "TypeReference"
                        },
                        "segment": "Stream"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "a"
                    },
                    {
                        "annotation": {
                            "tag": "AbilityBraces"
                        },
                        "segment": "}"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "contents": "#nirp5os0q69o4e1u9p3t6mmq6l6otluefi3ksm7dhm0diidjvkkgl8o9bvnflbj0sanuvdusf34f1qrins3ktcaglpcqv9oums2slsg",
                            "tag": "TypeReference"
                        },
                        "segment": "Optional"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "t"
                    },
                    {
                        "annotation": null,
                        "segment": "\n"
                    },
                    {
                        "annotation": {
                            "contents": "take",
                            "tag": "HashQualifier"
                        },
                        "segment": "take"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "n"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "s"
                    },
                    {
                        "annotation": {
                            "tag": "BindingEquals"
                        },
                        "segment": " ="
                    },
                    {
                        "annotation": null,
                        "segment": "\n"
                    },
                    {
                        "annotation": null,
                        "segment": "  "
                    },
                    {
                        "annotation": {
                            "tag": "UseKeyword"
                        },
                        "segment": "use "
                    },
                    {
                        "annotation": {
                            "tag": "UsePrefix"
                        },
                        "segment": "Nat"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "UseSuffix"
                        },
                        "segment": "-"
                    },
                    {
                        "annotation": {
                            "tag": "UseSuffix"
                        },
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "UseSuffix"
                        },
                        "segment": ">"
                    },
                    {
                        "annotation": null,
                        "segment": "\n"
                    },
                    {
                        "annotation": null,
                        "segment": "  "
                    },
                    {
                        "annotation": {
                            "contents": "h",
                            "tag": "HashQualifier"
                        },
                        "segment": "h"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "n"
                    },
                    {
                        "annotation": {
                            "tag": "BindingEquals"
                        },
                        "segment": " ="
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "ControlKeyword"
                        },
                        "segment": "cases"
                    },
                    {
                        "annotation": null,
                        "segment": "\n"
                    },
                    {
                        "annotation": null,
                        "segment": "  "
                    },
                    {
                        "annotation": null,
                        "segment": "  "
                    },
                    {
                        "annotation": {
                            "tag": "DelimiterChar"
                        },
                        "segment": "{"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "contents": "#b035k0tpdv9jbs80ig29hujmv9kpkubda6or4320o5g7aj7edsudislnp2uovntgu5b0e6a18p0p7j8r2hcpr20blls7am8nll6t2ro#a0",
                            "tag": "TermReference"
                        },
                        "segment": "emit"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "a"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "ControlKeyword"
                        },
                        "segment": "->"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "k"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "DelimiterChar"
                        },
                        "segment": "}"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "ControlKeyword"
                        },
                        "segment": "->"
                    },
                    {
                        "annotation": null,
                        "segment": "\n"
                    },
                    {
                        "annotation": null,
                        "segment": "  "
                    },
                    {
                        "annotation": null,
                        "segment": "  "
                    },
                    {
                        "annotation": null,
                        "segment": "  "
                    }
                ]
            },
            {
                "diffTag": "old",
                "elements": [
                    {
                        "annotation": {
                            "tag": "ControlKeyword"
                        },
                        "segment": "if "
                    },
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "n"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "contents": "##Nat.>",
                            "tag": "TermReference"
                        },
                        "segment": ">"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "NumericLiteral"
                        },
                        "segment": "0"
                    },
                    {
                        "annotation": {
                            "tag": "ControlKeyword"
                        },
                        "segment": " then"
                    },
                    {
                        "annotation": null,
                        "segment": "\n"
                    },
                    {
                        "annotation": null,
                        "segment": "  "
                    },
                    {
                        "annotation": null,
                        "segment": "  "
                    },
                    {
                        "annotation": null,
                        "segment": "  "
                    },
                    {
                        "annotation": null,
                        "segment": "  "
                    }
                ]
            },
            {
                "diffTag": "both",
                "elements": [
                    {
                        "annotation": {
                            "contents": "#b035k0tpdv9jbs80ig29hujmv9kpkubda6or4320o5g7aj7edsudislnp2uovntgu5b0e6a18p0p7j8r2hcpr20blls7am8nll6t2ro#a0",
                            "tag": "TermReference"
                        },
                        "segment": "emit"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "a"
                    },
                    {
                        "annotation": null,
                        "segment": "\n"
                    },
                    {
                        "annotation": null,
                        "segment": "  "
                    },
                    {
                        "annotation": null,
                        "segment": "  "
                    },
                    {
                        "annotation": null,
                        "segment": "  "
                    }
                ]
            },
            {
                "diffTag": "old",
                "elements": [
                    {
                        "annotation": null,
                        "segment": "  "
                    }
                ]
            },
            {
                "diffTag": "new",
                "elements": [
                    {
                        "annotation": {
                            "tag": "ControlKeyword"
                        },
                        "segment": "if"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "n"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "contents": "##Nat.>",
                            "tag": "TermReference"
                        },
                        "segment": ">"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "NumericLiteral"
                        },
                        "segment": "0"
                    },
                    {
                        "annotation": {
                            "tag": "ControlKeyword"
                        },
                        "segment": " then"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    }
                ]
            },
            {
                "diffTag": "both",
                "elements": [
                    {
                        "annotation": {
                            "tag": "ControlKeyword"
                        },
                        "segment": "handle"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "k"
                    },
                    {
                        "annotation": {
                            "tag": "Unit"
                        },
                        "segment": "()"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "ControlKeyword"
                        },
                        "segment": "with"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "h"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "Parenthesis"
                        },
                        "segment": "("
                    },
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "n"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "contents": "##Nat.drop",
                            "tag": "TermReference"
                        },
                        "segment": "-"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "NumericLiteral"
                        },
                        "segment": "1"
                    },
                    {
                        "annotation": {
                            "tag": "Parenthesis"
                        },
                        "segment": ")"
                    }
                ]
            },
            {
                "diffTag": "old",
                "elements": [
                    {
                        "annotation": null,
                        "segment": "\n"
                    },
                    {
                        "annotation": null,
                        "segment": "  "
                    },
                    {
                        "annotation": null,
                        "segment": "  "
                    },
                    {
                        "annotation": null,
                        "segment": "  "
                    }
                ]
            },
            {
                "diffTag": "new",
                "elements": [
                    {
                        "annotation": null,
                        "segment": " "
                    }
                ]
            },
            {
                "diffTag": "both",
                "elements": [
                    {
                        "annotation": {
                            "tag": "ControlKeyword"
                        },
                        "segment": "else"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "contents": "#nirp5os0q69o4e1u9p3t6mmq6l6otluefi3ksm7dhm0diidjvkkgl8o9bvnflbj0sanuvdusf34f1qrins3ktcaglpcqv9oums2slsg#d1",
                            "tag": "TermReference"
                        },
                        "segment": "None"
                    },
                    {
                        "annotation": null,
                        "segment": "\n"
                    },
                    {
                        "annotation": null,
                        "segment": "  "
                    },
                    {
                        "annotation": null,
                        "segment": "  "
                    },
                    {
                        "annotation": {
                            "tag": "DelimiterChar"
                        },
                        "segment": "{"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "r"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "DelimiterChar"
                        },
                        "segment": "}"
                    },
                    {
                        "annotation": null,
                        "segment": "           "
                    },
                    {
                        "annotation": {
                            "tag": "ControlKeyword"
                        },
                        "segment": "->"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "contents": "#nirp5os0q69o4e1u9p3t6mmq6l6otluefi3ksm7dhm0diidjvkkgl8o9bvnflbj0sanuvdusf34f1qrins3ktcaglpcqv9oums2slsg#d0",
                            "tag": "TermReference"
                        },
                        "segment": "Some"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "r"
                    },
                    {
                        "annotation": null,
                        "segment": "\n"
                    },
                    {
                        "annotation": null,
                        "segment": "  "
                    }
                ]
            },
            {
                "diffTag": "new",
                "elements": [
                    {
                        "annotation": {
                            "tag": "ControlKeyword"
                        },
                        "segment": "if"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "n"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "contents": "##Nat.>",
                            "tag": "TermReference"
                        },
                        "segment": ">"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "NumericLiteral"
                        },
                        "segment": "0"
                    },
                    {
                        "annotation": {
                            "tag": "ControlKeyword"
                        },
                        "segment": " then"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    }
                ]
            },
            {
                "diffTag": "both",
                "elements": [
                    {
                        "annotation": {
                            "tag": "ControlKeyword"
                        },
                        "segment": "handle"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "s"
                    },
                    {
                        "annotation": {
                            "tag": "Unit"
                        },
                        "segment": "()"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "ControlKeyword"
                        },
                        "segment": "with"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "h"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    }
                ]
            },
            {
                "diffTag": "new",
                "elements": [
                    {
                        "annotation": {
                            "tag": "Parenthesis"
                        },
                        "segment": "("
                    }
                ]
            },
            {
                "diffTag": "both",
                "elements": [
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "n"
                    }
                ]
            },
            {
                "diffTag": "new",
                "elements": [
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "contents": "##Nat.drop",
                            "tag": "TermReference"
                        },
                        "segment": "-"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "NumericLiteral"
                        },
                        "segment": "1"
                    },
                    {
                        "annotation": {
                            "tag": "Parenthesis"
                        },
                        "segment": ")"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "ControlKeyword"
                        },
                        "segment": "else"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "contents": "#nirp5os0q69o4e1u9p3t6mmq6l6otluefi3ksm7dhm0diidjvkkgl8o9bvnflbj0sanuvdusf34f1qrins3ktcaglpcqv9oums2slsg#d1",
                            "tag": "TermReference"
                        },
                        "segment": "None"
                    }
                ]
            }
        ],
        "tag": "UserObject"
    },
    "diffKind": "diff",
    "newBranchRef": "new",
    "newTerm": {
        "bestTermName": "take",
        "defnTermTag": "Plain",
        "signature": [
            {
                "annotation": {
                    "contents": "##Nat",
                    "tag": "TypeReference"
                },
                "segment": "Nat"
            },
            {
                "annotation": null,
                "segment": " "
            },
            {
                "annotation": {
                    "tag": "TypeOperator"
                },
                "segment": "->"
            },
            {
                "annotation": null,
                "segment": " "
            },
            {
                "annotation": {
                    "tag": "DelayForceChar"
                },
                "segment": "'"
            },
            {
                "annotation": {
                    "tag": "AbilityBraces"
                },
                "segment": "{"
            },
            {
                "annotation": {
                    "tag": "Var"
                },
                "segment": "g"
            },
            {
                "annotation": {
                    "tag": "AbilityBraces"
                },
                "segment": "}"
            },
            {
                "annotation": null,
                "segment": " "
            },
            {
                "annotation": {
                    "tag": "Var"
                },
                "segment": "t"
            },
            {
                "annotation": null,
                "segment": " "
            },
            {
                "annotation": {
                    "tag": "TypeOperator"
                },
                "segment": "->"
            },
            {
                "annotation": {
                    "tag": "AbilityBraces"
                },
                "segment": "{"
            },
            {
                "annotation": {
                    "tag": "Var"
                },
                "segment": "g"
            },
            {
                "annotation": null,
                "segment": ","
            },
            {
                "annotation": null,
                "segment": " "
            },
            {
                "annotation": {
                    "contents": "#b035k0tpdv9jbs80ig29hujmv9kpkubda6or4320o5g7aj7edsudislnp2uovntgu5b0e6a18p0p7j8r2hcpr20blls7am8nll6t2ro",
                    "tag": "TypeReference"
                },
                "segment": "Stream"
            },
            {
                "annotation": null,
                "segment": " "
            },
            {
                "annotation": {
                    "tag": "Var"
                },
                "segment": "a"
            },
            {
                "annotation": {
                    "tag": "AbilityBraces"
                },
                "segment": "}"
            },
            {
                "annotation": null,
                "segment": " "
            },
            {
                "annotation": {
                    "contents": "#nirp5os0q69o4e1u9p3t6mmq6l6otluefi3ksm7dhm0diidjvkkgl8o9bvnflbj0sanuvdusf34f1qrins3ktcaglpcqv9oums2slsg",
                    "tag": "TypeReference"
                },
                "segment": "Optional"
            },
            {
                "annotation": null,
                "segment": " "
            },
            {
                "annotation": {
                    "tag": "Var"
                },
                "segment": "t"
            }
        ],
        "termDefinition": {
            "contents": [
                {
                    "annotation": {
                        "contents": "take",
                        "tag": "HashQualifier"
                    },
                    "segment": "take"
                },
                {
                    "annotation": {
                        "tag": "TypeAscriptionColon"
                    },
                    "segment": " :"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "##Nat",
                        "tag": "TypeReference"
                    },
                    "segment": "Nat"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "TypeOperator"
                    },
                    "segment": "->"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "DelayForceChar"
                    },
                    "segment": "'"
                },
                {
                    "annotation": {
                        "tag": "AbilityBraces"
                    },
                    "segment": "{"
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "g"
                },
                {
                    "annotation": {
                        "tag": "AbilityBraces"
                    },
                    "segment": "}"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "t"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "TypeOperator"
                    },
                    "segment": "->"
                },
                {
                    "annotation": {
                        "tag": "AbilityBraces"
                    },
                    "segment": "{"
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "g"
                },
                {
                    "annotation": null,
                    "segment": ","
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "#b035k0tpdv9jbs80ig29hujmv9kpkubda6or4320o5g7aj7edsudislnp2uovntgu5b0e6a18p0p7j8r2hcpr20blls7am8nll6t2ro",
                        "tag": "TypeReference"
                    },
                    "segment": "Stream"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "a"
                },
                {
                    "annotation": {
                        "tag": "AbilityBraces"
                    },
                    "segment": "}"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "#nirp5os0q69o4e1u9p3t6mmq6l6otluefi3ksm7dhm0diidjvkkgl8o9bvnflbj0sanuvdusf34f1qrins3ktcaglpcqv9oums2slsg",
                        "tag": "TypeReference"
                    },
                    "segment": "Optional"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "t"
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": {
                        "contents": "take",
                        "tag": "HashQualifier"
                    },
                    "segment": "take"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "n"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "s"
                },
                {
                    "annotation": {
                        "tag": "BindingEquals"
                    },
                    "segment": " ="
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "tag": "UseKeyword"
                    },
                    "segment": "use "
                },
                {
                    "annotation": {
                        "tag": "UsePrefix"
                    },
                    "segment": "Nat"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "UseSuffix"
                    },
                    "segment": "-"
                },
                {
                    "annotation": {
                        "tag": "UseSuffix"
                    },
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "UseSuffix"
                    },
                    "segment": ">"
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "contents": "h",
                        "tag": "HashQualifier"
                    },
                    "segment": "h"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "n"
                },
                {
                    "annotation": {
                        "tag": "BindingEquals"
                    },
                    "segment": " ="
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "ControlKeyword"
                    },
                    "segment": "cases"
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "tag": "DelimiterChar"
                    },
                    "segment": "{"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "#b035k0tpdv9jbs80ig29hujmv9kpkubda6or4320o5g7aj7edsudislnp2uovntgu5b0e6a18p0p7j8r2hcpr20blls7am8nll6t2ro#a0",
                        "tag": "TermReference"
                    },
                    "segment": "emit"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "a"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "ControlKeyword"
                    },
                    "segment": "->"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "k"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "DelimiterChar"
                    },
                    "segment": "}"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "ControlKeyword"
                    },
                    "segment": "->"
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "contents": "#b035k0tpdv9jbs80ig29hujmv9kpkubda6or4320o5g7aj7edsudislnp2uovntgu5b0e6a18p0p7j8r2hcpr20blls7am8nll6t2ro#a0",
                        "tag": "TermReference"
                    },
                    "segment": "emit"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "a"
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "tag": "ControlKeyword"
                    },
                    "segment": "if"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "n"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "##Nat.>",
                        "tag": "TermReference"
                    },
                    "segment": ">"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "NumericLiteral"
                    },
                    "segment": "0"
                },
                {
                    "annotation": {
                        "tag": "ControlKeyword"
                    },
                    "segment": " then"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "ControlKeyword"
                    },
                    "segment": "handle"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "k"
                },
                {
                    "annotation": {
                        "tag": "Unit"
                    },
                    "segment": "()"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "ControlKeyword"
                    },
                    "segment": "with"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "h"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Parenthesis"
                    },
                    "segment": "("
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "n"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "##Nat.drop",
                        "tag": "TermReference"
                    },
                    "segment": "-"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "NumericLiteral"
                    },
                    "segment": "1"
                },
                {
                    "annotation": {
                        "tag": "Parenthesis"
                    },
                    "segment": ")"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "ControlKeyword"
                    },
                    "segment": "else"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "#nirp5os0q69o4e1u9p3t6mmq6l6otluefi3ksm7dhm0diidjvkkgl8o9bvnflbj0sanuvdusf34f1qrins3ktcaglpcqv9oums2slsg#d1",
                        "tag": "TermReference"
                    },
                    "segment": "None"
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "tag": "DelimiterChar"
                    },
                    "segment": "{"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "r"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "DelimiterChar"
                    },
                    "segment": "}"
                },
                {
                    "annotation": null,
                    "segment": "           "
                },
                {
                    "annotation": {
                        "tag": "ControlKeyword"
                    },
                    "segment": "->"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "#nirp5os0q69o4e1u9p3t6mmq6l6otluefi3ksm7dhm0diidjvkkgl8o9bvnflbj0sanuvdusf34f1qrins3ktcaglpcqv9oums2slsg#d0",
                        "tag": "TermReference"
                    },
                    "segment": "Some"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "r"
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "tag": "ControlKeyword"
                    },
                    "segment": "if"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "n"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "##Nat.>",
                        "tag": "TermReference"
                    },
                    "segment": ">"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "NumericLiteral"
                    },
                    "segment": "0"
                },
                {
                    "annotation": {
                        "tag": "ControlKeyword"
                    },
                    "segment": " then"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "ControlKeyword"
                    },
                    "segment": "handle"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "s"
                },
                {
                    "annotation": {
                        "tag": "Unit"
                    },
                    "segment": "()"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "ControlKeyword"
                    },
                    "segment": "with"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "h"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Parenthesis"
                    },
                    "segment": "("
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "n"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "##Nat.drop",
                        "tag": "TermReference"
                    },
                    "segment": "-"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "NumericLiteral"
                    },
                    "segment": "1"
                },
                {
                    "annotation": {
                        "tag": "Parenthesis"
                    },
                    "segment": ")"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "ControlKeyword"
                    },
                    "segment": "else"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "#nirp5os0q69o4e1u9p3t6mmq6l6otluefi3ksm7dhm0diidjvkkgl8o9bvnflbj0sanuvdusf34f1qrins3ktcaglpcqv9oums2slsg#d1",
                        "tag": "TermReference"
                    },
                    "segment": "None"
                }
            ],
            "tag": "UserObject"
        },
        "termDocs": [],
        "termNames": [
            "take"
        ]
    },
    "oldBranchRef": "main",
    "oldTerm": {
        "bestTermName": "take",
        "defnTermTag": "Plain",
        "signature": [
            {
                "annotation": {
                    "contents": "##Nat",
                    "tag": "TypeReference"
                },
                "segment": "Nat"
            },
            {
                "annotation": null,
                "segment": " "
            },
            {
                "annotation": {
                    "tag": "TypeOperator"
                },
                "segment": "->"
            },
            {
                "annotation": null,
                "segment": " "
            },
            {
                "annotation": {
                    "tag": "DelayForceChar"
                },
                "segment": "'"
            },
            {
                "annotation": {
                    "tag": "AbilityBraces"
                },
                "segment": "{"
            },
            {
                "annotation": {
                    "tag": "Var"
                },
                "segment": "g"
            },
            {
                "annotation": {
                    "tag": "AbilityBraces"
                },
                "segment": "}"
            },
            {
                "annotation": null,
                "segment": " "
            },
            {
                "annotation": {
                    "tag": "Var"
                },
                "segment": "t"
            },
            {
                "annotation": null,
                "segment": " "
            },
            {
                "annotation": {
                    "tag": "TypeOperator"
                },
                "segment": "->"
            },
            {
                "annotation": {
                    "tag": "AbilityBraces"
                },
                "segment": "{"
            },
            {
                "annotation": {
                    "tag": "Var"
                },
                "segment": "g"
            },
            {
                "annotation": null,
                "segment": ","
            },
            {
                "annotation": null,
                "segment": " "
            },
            {
                "annotation": {
                    "contents": "#b035k0tpdv9jbs80ig29hujmv9kpkubda6or4320o5g7aj7edsudislnp2uovntgu5b0e6a18p0p7j8r2hcpr20blls7am8nll6t2ro",
                    "tag": "TypeReference"
                },
                "segment": "Stream"
            },
            {
                "annotation": null,
                "segment": " "
            },
            {
                "annotation": {
                    "tag": "Var"
                },
                "segment": "a"
            },
            {
                "annotation": {
                    "tag": "AbilityBraces"
                },
                "segment": "}"
            },
            {
                "annotation": null,
                "segment": " "
            },
            {
                "annotation": {
                    "contents": "#nirp5os0q69o4e1u9p3t6mmq6l6otluefi3ksm7dhm0diidjvkkgl8o9bvnflbj0sanuvdusf34f1qrins3ktcaglpcqv9oums2slsg",
                    "tag": "TypeReference"
                },
                "segment": "Optional"
            },
            {
                "annotation": null,
                "segment": " "
            },
            {
                "annotation": {
                    "tag": "Var"
                },
                "segment": "t"
            }
        ],
        "termDefinition": {
            "contents": [
                {
                    "annotation": {
                        "contents": "take",
                        "tag": "HashQualifier"
                    },
                    "segment": "take"
                },
                {
                    "annotation": {
                        "tag": "TypeAscriptionColon"
                    },
                    "segment": " :"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "##Nat",
                        "tag": "TypeReference"
                    },
                    "segment": "Nat"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "TypeOperator"
                    },
                    "segment": "->"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "DelayForceChar"
                    },
                    "segment": "'"
                },
                {
                    "annotation": {
                        "tag": "AbilityBraces"
                    },
                    "segment": "{"
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "g"
                },
                {
                    "annotation": {
                        "tag": "AbilityBraces"
                    },
                    "segment": "}"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "t"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "TypeOperator"
                    },
                    "segment": "->"
                },
                {
                    "annotation": {
                        "tag": "AbilityBraces"
                    },
                    "segment": "{"
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "g"
                },
                {
                    "annotation": null,
                    "segment": ","
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "#b035k0tpdv9jbs80ig29hujmv9kpkubda6or4320o5g7aj7edsudislnp2uovntgu5b0e6a18p0p7j8r2hcpr20blls7am8nll6t2ro",
                        "tag": "TypeReference"
                    },
                    "segment": "Stream"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "a"
                },
                {
                    "annotation": {
                        "tag": "AbilityBraces"
                    },
                    "segment": "}"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "#nirp5os0q69o4e1u9p3t6mmq6l6otluefi3ksm7dhm0diidjvkkgl8o9bvnflbj0sanuvdusf34f1qrins3ktcaglpcqv9oums2slsg",
                        "tag": "TypeReference"
                    },
                    "segment": "Optional"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "t"
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": {
                        "contents": "take",
                        "tag": "HashQualifier"
                    },
                    "segment": "take"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "n"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "s"
                },
                {
                    "annotation": {
                        "tag": "BindingEquals"
                    },
                    "segment": " ="
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "tag": "UseKeyword"
                    },
                    "segment": "use "
                },
                {
                    "annotation": {
                        "tag": "UsePrefix"
                    },
                    "segment": "Nat"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "UseSuffix"
                    },
                    "segment": "-"
                },
                {
                    "annotation": {
                        "tag": "UseSuffix"
                    },
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "UseSuffix"
                    },
                    "segment": ">"
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "contents": "h",
                        "tag": "HashQualifier"
                    },
                    "segment": "h"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "n"
                },
                {
                    "annotation": {
                        "tag": "BindingEquals"
                    },
                    "segment": " ="
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "ControlKeyword"
                    },
                    "segment": "cases"
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "tag": "DelimiterChar"
                    },
                    "segment": "{"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "#b035k0tpdv9jbs80ig29hujmv9kpkubda6or4320o5g7aj7edsudislnp2uovntgu5b0e6a18p0p7j8r2hcpr20blls7am8nll6t2ro#a0",
                        "tag": "TermReference"
                    },
                    "segment": "emit"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "a"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "ControlKeyword"
                    },
                    "segment": "->"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "k"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "DelimiterChar"
                    },
                    "segment": "}"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "ControlKeyword"
                    },
                    "segment": "->"
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "tag": "ControlKeyword"
                    },
                    "segment": "if "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "n"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "##Nat.>",
                        "tag": "TermReference"
                    },
                    "segment": ">"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "NumericLiteral"
                    },
                    "segment": "0"
                },
                {
                    "annotation": {
                        "tag": "ControlKeyword"
                    },
                    "segment": " then"
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "contents": "#b035k0tpdv9jbs80ig29hujmv9kpkubda6or4320o5g7aj7edsudislnp2uovntgu5b0e6a18p0p7j8r2hcpr20blls7am8nll6t2ro#a0",
                        "tag": "TermReference"
                    },
                    "segment": "emit"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "a"
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "tag": "ControlKeyword"
                    },
                    "segment": "handle"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "k"
                },
                {
                    "annotation": {
                        "tag": "Unit"
                    },
                    "segment": "()"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "ControlKeyword"
                    },
                    "segment": "with"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "h"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Parenthesis"
                    },
                    "segment": "("
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "n"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "##Nat.drop",
                        "tag": "TermReference"
                    },
                    "segment": "-"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "NumericLiteral"
                    },
                    "segment": "1"
                },
                {
                    "annotation": {
                        "tag": "Parenthesis"
                    },
                    "segment": ")"
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "tag": "ControlKeyword"
                    },
                    "segment": "else"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "#nirp5os0q69o4e1u9p3t6mmq6l6otluefi3ksm7dhm0diidjvkkgl8o9bvnflbj0sanuvdusf34f1qrins3ktcaglpcqv9oums2slsg#d1",
                        "tag": "TermReference"
                    },
                    "segment": "None"
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "tag": "DelimiterChar"
                    },
                    "segment": "{"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "r"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "DelimiterChar"
                    },
                    "segment": "}"
                },
                {
                    "annotation": null,
                    "segment": "           "
                },
                {
                    "annotation": {
                        "tag": "ControlKeyword"
                    },
                    "segment": "->"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "#nirp5os0q69o4e1u9p3t6mmq6l6otluefi3ksm7dhm0diidjvkkgl8o9bvnflbj0sanuvdusf34f1qrins3ktcaglpcqv9oums2slsg#d0",
                        "tag": "TermReference"
                    },
                    "segment": "Some"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "r"
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "tag": "ControlKeyword"
                    },
                    "segment": "handle"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "s"
                },
                {
                    "annotation": {
                        "tag": "Unit"
                    },
                    "segment": "()"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "ControlKeyword"
                    },
                    "segment": "with"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "h"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "n"
                }
            ],
            "tag": "UserObject"
        },
        "termDocs": [],
        "termNames": [
            "take"
        ]
    },
    "project": "diffs"
}
```

Regression test

``` api
GET /api/projects/diffs/diff/terms?oldBranchRef=main&newBranchRef=new&oldTerm=handleRequest&newTerm=handleRequest
{
    "diff": {
        "contents": [
            {
                "diffTag": "both",
                "elements": [
                    {
                        "annotation": {
                            "contents": "handleRequest",
                            "tag": "HashQualifier"
                        },
                        "segment": "handleRequest"
                    },
                    {
                        "annotation": {
                            "tag": "TypeAscriptionColon"
                        },
                        "segment": " :"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": null,
                        "segment": "("
                    },
                    {
                        "annotation": null,
                        "segment": ")"
                    },
                    {
                        "annotation": null,
                        "segment": "\n"
                    },
                    {
                        "annotation": {
                            "contents": "handleRequest",
                            "tag": "HashQualifier"
                        },
                        "segment": "handleRequest"
                    },
                    {
                        "annotation": {
                            "tag": "BindingEquals"
                        },
                        "segment": " ="
                    },
                    {
                        "annotation": null,
                        "segment": "\n"
                    },
                    {
                        "annotation": null,
                        "segment": "  "
                    },
                    {
                        "annotation": {
                            "contents": "finalizers",
                            "tag": "HashQualifier"
                        },
                        "segment": "finalizers"
                    },
                    {
                        "annotation": {
                            "tag": "BindingEquals"
                        },
                        "segment": " ="
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "contents": "##Sequence",
                            "tag": "TypeReference"
                        },
                        "segment": "["
                    },
                    {
                        "annotation": {
                            "tag": "NumericLiteral"
                        },
                        "segment": "1"
                    },
                    {
                        "annotation": {
                            "contents": "##Sequence",
                            "tag": "TypeReference"
                        },
                        "segment": ", "
                    },
                    {
                        "annotation": {
                            "tag": "NumericLiteral"
                        },
                        "segment": "2"
                    },
                    {
                        "annotation": {
                            "contents": "##Sequence",
                            "tag": "TypeReference"
                        },
                        "segment": ", "
                    },
                    {
                        "annotation": {
                            "tag": "NumericLiteral"
                        },
                        "segment": "3"
                    },
                    {
                        "annotation": {
                            "contents": "##Sequence",
                            "tag": "TypeReference"
                        },
                        "segment": "]"
                    },
                    {
                        "annotation": null,
                        "segment": "\n"
                    },
                    {
                        "annotation": null,
                        "segment": "  "
                    },
                    {
                        "annotation": {
                            "contents": "addFinalizer",
                            "tag": "HashQualifier"
                        },
                        "segment": "addFinalizer"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "f"
                    },
                    {
                        "annotation": {
                            "tag": "BindingEquals"
                        },
                        "segment": " ="
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "contents": "#a85req9b0u8gkt82fgebosrcu3ba5g1aoqgt1vu5ohd93vpbdlo184e9pf9hgc4nml73aeohru6enhnnpch5oqilutaf0h40uv8dfvg",
                            "tag": "TermReference"
                        },
                        "segment": "fakeRefModify"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "finalizers"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "Parenthesis"
                        },
                        "segment": "("
                    },
                    {
                        "annotation": null,
                        "segment": "fs"
                    },
                    {
                        "annotation": {
                            "tag": "ControlKeyword"
                        },
                        "segment": " ->"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    }
                ]
            },
            {
                "diffTag": "new",
                "elements": [
                    {
                        "annotation": {
                            "contents": "#2lg4ah6ir6t129m33d7gssnigacral39qdamo20mn6r2vefliubpeqnjhejai9ekjckv0qnu9mlu3k9nbpfhl2schec4dohn7rjhjt8",
                            "tag": "TypeReference"
                        },
                        "segment": "("
                    }
                ]
            },
            {
                "diffTag": "both",
                "elements": [
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "f"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "contents": "##List.cons",
                            "tag": "TermReference"
                        },
                        "segment": "+:"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "fs"
                    }
                ]
            },
            {
                "diffTag": "new",
                "elements": [
                    {
                        "annotation": {
                            "contents": "#2lg4ah6ir6t129m33d7gssnigacral39qdamo20mn6r2vefliubpeqnjhejai9ekjckv0qnu9mlu3k9nbpfhl2schec4dohn7rjhjt8",
                            "tag": "TypeReference"
                        },
                        "segment": ", "
                    },
                    {
                        "annotation": {
                            "contents": "#2lg4ah6ir6t129m33d7gssnigacral39qdamo20mn6r2vefliubpeqnjhejai9ekjckv0qnu9mlu3k9nbpfhl2schec4dohn7rjhjt8",
                            "tag": "TypeReference"
                        },
                        "segment": "("
                    }
                ]
            },
            {
                "diffTag": "both",
                "elements": [
                    {
                        "annotation": {
                            "contents": "#2lg4ah6ir6t129m33d7gssnigacral39qdamo20mn6r2vefliubpeqnjhejai9ekjckv0qnu9mlu3k9nbpfhl2schec4dohn7rjhjt8",
                            "tag": "TypeReference"
                        },
                        "segment": ")"
                    }
                ]
            },
            {
                "diffTag": "new",
                "elements": [
                    {
                        "annotation": {
                            "contents": "#2lg4ah6ir6t129m33d7gssnigacral39qdamo20mn6r2vefliubpeqnjhejai9ekjckv0qnu9mlu3k9nbpfhl2schec4dohn7rjhjt8",
                            "tag": "TypeReference"
                        },
                        "segment": ")"
                    },
                    {
                        "annotation": {
                            "tag": "Parenthesis"
                        },
                        "segment": ")"
                    }
                ]
            },
            {
                "diffTag": "both",
                "elements": [
                    {
                        "annotation": null,
                        "segment": "\n"
                    },
                    {
                        "annotation": null,
                        "segment": "  "
                    }
                ]
            },
            {
                "diffTag": "annotationChange",
                "fromAnnotation": {
                    "contents": "#bcar3v5qe5466tnl5vc1crcpo13mv0pbspfmtm0g9d9i66pp3og6f75bmk6bhv7ah09igb3un5pmdjdo5ghm0n6krnbne7u2ngi770g",
                    "tag": "TermReference"
                },
                "segment": "foreach",
                "toAnnotation": {
                    "contents": "#jb1dd16mkieu352mk4ijml6ksvobs3e31b6q0mt219rrnk9dt6o7rgs87b3kglpfo27nsqmu8ts4q8e55t44e6v894kg9d4361gj4po",
                    "tag": "TermReference"
                }
            },
            {
                "diffTag": "both",
                "elements": [
                    {
                        "annotation": null,
                        "segment": " "
                    }
                ]
            },
            {
                "diffTag": "new",
                "elements": [
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "finalizers"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    }
                ]
            },
            {
                "diffTag": "both",
                "elements": [
                    {
                        "annotation": {
                            "tag": "Parenthesis"
                        },
                        "segment": "("
                    },
                    {
                        "annotation": null,
                        "segment": "f"
                    },
                    {
                        "annotation": {
                            "tag": "ControlKeyword"
                        },
                        "segment": " ->"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "contents": "#2lg4ah6ir6t129m33d7gssnigacral39qdamo20mn6r2vefliubpeqnjhejai9ekjckv0qnu9mlu3k9nbpfhl2schec4dohn7rjhjt8",
                            "tag": "TypeReference"
                        },
                        "segment": "("
                    },
                    {
                        "annotation": {
                            "contents": "#2lg4ah6ir6t129m33d7gssnigacral39qdamo20mn6r2vefliubpeqnjhejai9ekjckv0qnu9mlu3k9nbpfhl2schec4dohn7rjhjt8",
                            "tag": "TypeReference"
                        },
                        "segment": ")"
                    },
                    {
                        "annotation": {
                            "tag": "Parenthesis"
                        },
                        "segment": ")"
                    }
                ]
            },
            {
                "diffTag": "old",
                "elements": [
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "finalizers"
                    }
                ]
            }
        ],
        "tag": "UserObject"
    },
    "diffKind": "diff",
    "newBranchRef": "new",
    "newTerm": {
        "bestTermName": "handleRequest",
        "defnTermTag": "Plain",
        "signature": [
            {
                "annotation": null,
                "segment": "("
            },
            {
                "annotation": null,
                "segment": ")"
            }
        ],
        "termDefinition": {
            "contents": [
                {
                    "annotation": {
                        "contents": "handleRequest",
                        "tag": "HashQualifier"
                    },
                    "segment": "handleRequest"
                },
                {
                    "annotation": {
                        "tag": "TypeAscriptionColon"
                    },
                    "segment": " :"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": null,
                    "segment": "("
                },
                {
                    "annotation": null,
                    "segment": ")"
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": {
                        "contents": "handleRequest",
                        "tag": "HashQualifier"
                    },
                    "segment": "handleRequest"
                },
                {
                    "annotation": {
                        "tag": "BindingEquals"
                    },
                    "segment": " ="
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "contents": "finalizers",
                        "tag": "HashQualifier"
                    },
                    "segment": "finalizers"
                },
                {
                    "annotation": {
                        "tag": "BindingEquals"
                    },
                    "segment": " ="
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "##Sequence",
                        "tag": "TypeReference"
                    },
                    "segment": "["
                },
                {
                    "annotation": {
                        "tag": "NumericLiteral"
                    },
                    "segment": "1"
                },
                {
                    "annotation": {
                        "contents": "##Sequence",
                        "tag": "TypeReference"
                    },
                    "segment": ", "
                },
                {
                    "annotation": {
                        "tag": "NumericLiteral"
                    },
                    "segment": "2"
                },
                {
                    "annotation": {
                        "contents": "##Sequence",
                        "tag": "TypeReference"
                    },
                    "segment": ", "
                },
                {
                    "annotation": {
                        "tag": "NumericLiteral"
                    },
                    "segment": "3"
                },
                {
                    "annotation": {
                        "contents": "##Sequence",
                        "tag": "TypeReference"
                    },
                    "segment": "]"
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "contents": "addFinalizer",
                        "tag": "HashQualifier"
                    },
                    "segment": "addFinalizer"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "f"
                },
                {
                    "annotation": {
                        "tag": "BindingEquals"
                    },
                    "segment": " ="
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "#a85req9b0u8gkt82fgebosrcu3ba5g1aoqgt1vu5ohd93vpbdlo184e9pf9hgc4nml73aeohru6enhnnpch5oqilutaf0h40uv8dfvg",
                        "tag": "TermReference"
                    },
                    "segment": "fakeRefModify"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "finalizers"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Parenthesis"
                    },
                    "segment": "("
                },
                {
                    "annotation": null,
                    "segment": "fs"
                },
                {
                    "annotation": {
                        "tag": "ControlKeyword"
                    },
                    "segment": " ->"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "#2lg4ah6ir6t129m33d7gssnigacral39qdamo20mn6r2vefliubpeqnjhejai9ekjckv0qnu9mlu3k9nbpfhl2schec4dohn7rjhjt8",
                        "tag": "TypeReference"
                    },
                    "segment": "("
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "f"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "##List.cons",
                        "tag": "TermReference"
                    },
                    "segment": "+:"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "fs"
                },
                {
                    "annotation": {
                        "contents": "#2lg4ah6ir6t129m33d7gssnigacral39qdamo20mn6r2vefliubpeqnjhejai9ekjckv0qnu9mlu3k9nbpfhl2schec4dohn7rjhjt8",
                        "tag": "TypeReference"
                    },
                    "segment": ", "
                },
                {
                    "annotation": {
                        "contents": "#2lg4ah6ir6t129m33d7gssnigacral39qdamo20mn6r2vefliubpeqnjhejai9ekjckv0qnu9mlu3k9nbpfhl2schec4dohn7rjhjt8",
                        "tag": "TypeReference"
                    },
                    "segment": "("
                },
                {
                    "annotation": {
                        "contents": "#2lg4ah6ir6t129m33d7gssnigacral39qdamo20mn6r2vefliubpeqnjhejai9ekjckv0qnu9mlu3k9nbpfhl2schec4dohn7rjhjt8",
                        "tag": "TypeReference"
                    },
                    "segment": ")"
                },
                {
                    "annotation": {
                        "contents": "#2lg4ah6ir6t129m33d7gssnigacral39qdamo20mn6r2vefliubpeqnjhejai9ekjckv0qnu9mlu3k9nbpfhl2schec4dohn7rjhjt8",
                        "tag": "TypeReference"
                    },
                    "segment": ")"
                },
                {
                    "annotation": {
                        "tag": "Parenthesis"
                    },
                    "segment": ")"
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "contents": "#jb1dd16mkieu352mk4ijml6ksvobs3e31b6q0mt219rrnk9dt6o7rgs87b3kglpfo27nsqmu8ts4q8e55t44e6v894kg9d4361gj4po",
                        "tag": "TermReference"
                    },
                    "segment": "foreach"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "finalizers"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Parenthesis"
                    },
                    "segment": "("
                },
                {
                    "annotation": null,
                    "segment": "f"
                },
                {
                    "annotation": {
                        "tag": "ControlKeyword"
                    },
                    "segment": " ->"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "#2lg4ah6ir6t129m33d7gssnigacral39qdamo20mn6r2vefliubpeqnjhejai9ekjckv0qnu9mlu3k9nbpfhl2schec4dohn7rjhjt8",
                        "tag": "TypeReference"
                    },
                    "segment": "("
                },
                {
                    "annotation": {
                        "contents": "#2lg4ah6ir6t129m33d7gssnigacral39qdamo20mn6r2vefliubpeqnjhejai9ekjckv0qnu9mlu3k9nbpfhl2schec4dohn7rjhjt8",
                        "tag": "TypeReference"
                    },
                    "segment": ")"
                },
                {
                    "annotation": {
                        "tag": "Parenthesis"
                    },
                    "segment": ")"
                }
            ],
            "tag": "UserObject"
        },
        "termDocs": [],
        "termNames": [
            "handleRequest"
        ]
    },
    "oldBranchRef": "main",
    "oldTerm": {
        "bestTermName": "handleRequest",
        "defnTermTag": "Plain",
        "signature": [
            {
                "annotation": null,
                "segment": "("
            },
            {
                "annotation": null,
                "segment": ")"
            }
        ],
        "termDefinition": {
            "contents": [
                {
                    "annotation": {
                        "contents": "handleRequest",
                        "tag": "HashQualifier"
                    },
                    "segment": "handleRequest"
                },
                {
                    "annotation": {
                        "tag": "TypeAscriptionColon"
                    },
                    "segment": " :"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": null,
                    "segment": "("
                },
                {
                    "annotation": null,
                    "segment": ")"
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": {
                        "contents": "handleRequest",
                        "tag": "HashQualifier"
                    },
                    "segment": "handleRequest"
                },
                {
                    "annotation": {
                        "tag": "BindingEquals"
                    },
                    "segment": " ="
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "contents": "finalizers",
                        "tag": "HashQualifier"
                    },
                    "segment": "finalizers"
                },
                {
                    "annotation": {
                        "tag": "BindingEquals"
                    },
                    "segment": " ="
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "##Sequence",
                        "tag": "TypeReference"
                    },
                    "segment": "["
                },
                {
                    "annotation": {
                        "tag": "NumericLiteral"
                    },
                    "segment": "1"
                },
                {
                    "annotation": {
                        "contents": "##Sequence",
                        "tag": "TypeReference"
                    },
                    "segment": ", "
                },
                {
                    "annotation": {
                        "tag": "NumericLiteral"
                    },
                    "segment": "2"
                },
                {
                    "annotation": {
                        "contents": "##Sequence",
                        "tag": "TypeReference"
                    },
                    "segment": ", "
                },
                {
                    "annotation": {
                        "tag": "NumericLiteral"
                    },
                    "segment": "3"
                },
                {
                    "annotation": {
                        "contents": "##Sequence",
                        "tag": "TypeReference"
                    },
                    "segment": "]"
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "contents": "addFinalizer",
                        "tag": "HashQualifier"
                    },
                    "segment": "addFinalizer"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "f"
                },
                {
                    "annotation": {
                        "tag": "BindingEquals"
                    },
                    "segment": " ="
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "#a85req9b0u8gkt82fgebosrcu3ba5g1aoqgt1vu5ohd93vpbdlo184e9pf9hgc4nml73aeohru6enhnnpch5oqilutaf0h40uv8dfvg",
                        "tag": "TermReference"
                    },
                    "segment": "fakeRefModify"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "finalizers"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Parenthesis"
                    },
                    "segment": "("
                },
                {
                    "annotation": null,
                    "segment": "fs"
                },
                {
                    "annotation": {
                        "tag": "ControlKeyword"
                    },
                    "segment": " ->"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "f"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "##List.cons",
                        "tag": "TermReference"
                    },
                    "segment": "+:"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "fs"
                },
                {
                    "annotation": {
                        "tag": "Parenthesis"
                    },
                    "segment": ")"
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "contents": "#bcar3v5qe5466tnl5vc1crcpo13mv0pbspfmtm0g9d9i66pp3og6f75bmk6bhv7ah09igb3un5pmdjdo5ghm0n6krnbne7u2ngi770g",
                        "tag": "TermReference"
                    },
                    "segment": "foreach"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Parenthesis"
                    },
                    "segment": "("
                },
                {
                    "annotation": null,
                    "segment": "f"
                },
                {
                    "annotation": {
                        "tag": "ControlKeyword"
                    },
                    "segment": " ->"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "#2lg4ah6ir6t129m33d7gssnigacral39qdamo20mn6r2vefliubpeqnjhejai9ekjckv0qnu9mlu3k9nbpfhl2schec4dohn7rjhjt8",
                        "tag": "TypeReference"
                    },
                    "segment": "("
                },
                {
                    "annotation": {
                        "contents": "#2lg4ah6ir6t129m33d7gssnigacral39qdamo20mn6r2vefliubpeqnjhejai9ekjckv0qnu9mlu3k9nbpfhl2schec4dohn7rjhjt8",
                        "tag": "TypeReference"
                    },
                    "segment": ")"
                },
                {
                    "annotation": {
                        "tag": "Parenthesis"
                    },
                    "segment": ")"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "finalizers"
                }
            ],
            "tag": "UserObject"
        },
        "termDocs": [],
        "termNames": [
            "handleRequest"
        ]
    },
    "project": "diffs"
}
```

Diff types

``` api
GET /api/projects/diffs/diff/types?oldBranchRef=main&newBranchRef=new&oldType=Type&newType=Type
{
    "diff": {
        "contents": [
            {
                "diffTag": "both",
                "elements": [
                    {
                        "annotation": {
                            "tag": "DataTypeKeyword"
                        },
                        "segment": "type"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "contents": "Type",
                            "tag": "HashQualifier"
                        },
                        "segment": "Type"
                    }
                ]
            },
            {
                "diffTag": "new",
                "elements": [
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "DataTypeParams"
                        },
                        "segment": "a"
                    }
                ]
            },
            {
                "diffTag": "both",
                "elements": [
                    {
                        "annotation": {
                            "tag": "DelimiterChar"
                        },
                        "segment": " = "
                    }
                ]
            },
            {
                "diffTag": "annotationChange",
                "fromAnnotation": {
                    "contents": "#m5hlrmkn9a3kuqabta2e9qs934em1qmkotpsh9tjvta2u86nuesbjbk2k2sprbdiljq7uqibp49vku4gfpg2u60ceiv8net1f0bu2n8#d0",
                    "tag": "TermReference"
                },
                "segment": "Type",
                "toAnnotation": {
                    "contents": "#uik7pl3klg4u2obtf2fattdaeldui46ohmsi0knpp5hu8tn4d5o8vp570qgh7esgap0pmq9cfrh9dfg1r8qa7qh33g45a3tric24o20#d0",
                    "tag": "TermReference"
                }
            },
            {
                "diffTag": "both",
                "elements": [
                    {
                        "annotation": null,
                        "segment": " "
                    }
                ]
            },
            {
                "diffTag": "old",
                "elements": [
                    {
                        "annotation": {
                            "contents": "##Nat",
                            "tag": "TypeReference"
                        },
                        "segment": "Nat"
                    }
                ]
            },
            {
                "diffTag": "new",
                "elements": [
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "a"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "contents": "##Text",
                            "tag": "TypeReference"
                        },
                        "segment": "Text"
                    }
                ]
            }
        ],
        "tag": "UserObject"
    },
    "diffKind": "diff",
    "newBranchRef": "new",
    "newType": {
        "bestTypeName": "Type",
        "defnTypeTag": "Data",
        "typeDefinition": {
            "contents": [
                {
                    "annotation": {
                        "tag": "DataTypeKeyword"
                    },
                    "segment": "type"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "Type",
                        "tag": "HashQualifier"
                    },
                    "segment": "Type"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "DataTypeParams"
                    },
                    "segment": "a"
                },
                {
                    "annotation": {
                        "tag": "DelimiterChar"
                    },
                    "segment": " = "
                },
                {
                    "annotation": {
                        "contents": "#uik7pl3klg4u2obtf2fattdaeldui46ohmsi0knpp5hu8tn4d5o8vp570qgh7esgap0pmq9cfrh9dfg1r8qa7qh33g45a3tric24o20#d0",
                        "tag": "TermReference"
                    },
                    "segment": "Type"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "a"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "##Text",
                        "tag": "TypeReference"
                    },
                    "segment": "Text"
                }
            ],
            "tag": "UserObject"
        },
        "typeDocs": [],
        "typeNames": [
            "Type"
        ]
    },
    "oldBranchRef": "main",
    "oldType": {
        "bestTypeName": "Type",
        "defnTypeTag": "Data",
        "typeDefinition": {
            "contents": [
                {
                    "annotation": {
                        "tag": "DataTypeKeyword"
                    },
                    "segment": "type"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "Type",
                        "tag": "HashQualifier"
                    },
                    "segment": "Type"
                },
                {
                    "annotation": {
                        "tag": "DelimiterChar"
                    },
                    "segment": " = "
                },
                {
                    "annotation": {
                        "contents": "#m5hlrmkn9a3kuqabta2e9qs934em1qmkotpsh9tjvta2u86nuesbjbk2k2sprbdiljq7uqibp49vku4gfpg2u60ceiv8net1f0bu2n8#d0",
                        "tag": "TermReference"
                    },
                    "segment": "Type"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "##Nat",
                        "tag": "TypeReference"
                    },
                    "segment": "Nat"
                }
            ],
            "tag": "UserObject"
        },
        "typeDocs": [],
        "typeNames": [
            "Type"
        ]
    },
    "project": "diffs"
}
```


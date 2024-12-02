# Doc rendering

``` ucm :hide
scratch/main> builtins.mergeio
```

``` unison :hide
structural type Maybe a = Nothing | Just a
otherTerm = "text"

otherDoc : (Text -> Doc2) -> Doc2
otherDoc mkMsg = {{
This doc should be embedded.

{{mkMsg "message"}}

}}

{{
# Heading

## Heading 2

Term Link: {otherTerm}

Type Link: {type Maybe}

Term source:

@source{term}

Term signature:

@signature{term}

* List item

1. Numbered list item

> Block quote

    Code block

Inline code:

`` 1 + 2 ``

`"doesn't typecheck" + 1`

[Link](https://unison-lang.org)

![Image](https://share-next.unison-lang.org/static/unison-logo-circle.png)

**Bold**

*Italic*

~~Strikethrough~~

Horizontal rule

---

Table

| Header 1 | Header 2 |
| -------- | -------- |
| Cell 1   | Cell 2   |
| Cell 3   | Cell 4   |


Video

{{ Special (Embed (Any (Video [(MediaSource "test.mp4" None)] [("poster", "test.png")]))) }}

Transclusion/evaluation:

{{otherDoc (a -> Word a )}}

}}
term = 42
```

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> display term.doc

  # Heading

    # Heading 2
    
      Term Link: otherTerm
    
      Type Link: Maybe
    
      Term source:
    
          term : Nat
          term = 42
    
      Term signature:
    
          term : Nat
    
      * List item
    
      1. Numbered list item
    
      > Block quote
    
      Code block
    
      Inline code:
    
      `1 Nat.+ 2`
    
      `"doesn't typecheck" + 1`
    
      Link
    
      ![Image](https://share-next.unison-lang.org/static/unison-logo-circle.png)
    
      Bold
    
      Italic
    
      ~~Strikethrough~~
    
      Horizontal rule
    
      ---
    
      Table
    
      | Header 1 | Header 2 | | -------- | -------- | | Cell 1 |
      Cell 2 | | Cell 3 | Cell 4 |
    
      Video
    
          
           {{ embed {{
      Video
          [MediaSource "test.mp4" Nothing]
          [("poster", "test.png")] }} }}  
          
    
      Transclusion/evaluation:
    
      This doc should be embedded.
      
      message
```

``` api
GET /api/projects/scratch/branches/main/getDefinition?names=term
  {
      "missingDefinitions": [],
      "termDefinitions": {
          "#qkhkl0n238s1eqibd1ecb8605sqj1m4hpoaag177cu572otqlaf1u28c8suuuqgljdtthsjtr07rv04np05o6oa27ml9105k7uas0t8": {
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
                          "segment": " "
                      },
                      {
                          "annotation": {
                              "tag": "NumericLiteral"
                          },
                          "segment": "42"
                      }
                  ],
                  "tag": "UserObject"
              },
              "termDocs": [
                  [
                      "doc",
                      "#kjfaflbrgl89j2uq4ruubejakm6s02cp3m61ufu7rv7tkbd4nmkvcn1fciue53v0msir9t7ds111ab9er8qfa06gsa9ddfrdfgc99mo",
                      {
                          "contents": [
                              {
                                  "contents": [
                                      {
                                          "contents": "Heading",
                                          "tag": "Word"
                                      }
                                  ],
                                  "tag": "Paragraph"
                              },
                              [
                                  {
                                      "contents": [
                                          {
                                              "contents": [
                                                  {
                                                      "contents": "Heading",
                                                      "tag": "Word"
                                                  },
                                                  {
                                                      "contents": "2",
                                                      "tag": "Word"
                                                  }
                                              ],
                                              "tag": "Paragraph"
                                          },
                                          [
                                              {
                                                  "contents": [
                                                      {
                                                          "contents": "Term",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "Link:",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": {
                                                              "contents": [
                                                                  {
                                                                      "annotation": {
                                                                          "contents": "#k5gpql9cbdfau6lf1aja24joc3sfctvjor8esu8bemn0in3l148otb0t3vebgqrt6qml302h62bbfeftg65gec1v8ouin5m6v2969d8",
                                                                          "tag": "TermReference"
                                                                      },
                                                                      "segment": "otherTerm"
                                                                  }
                                                              ],
                                                              "tag": "Link"
                                                          },
                                                          "tag": "Special"
                                                      }
                                                  ],
                                                  "tag": "Paragraph"
                                              },
                                              {
                                                  "contents": [
                                                      {
                                                          "contents": "Type",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "Link:",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": {
                                                              "contents": [
                                                                  {
                                                                      "annotation": {
                                                                          "contents": "#nirp5os0q69o4e1u9p3t6mmq6l6otluefi3ksm7dhm0diidjvkkgl8o9bvnflbj0sanuvdusf34f1qrins3ktcaglpcqv9oums2slsg",
                                                                          "tag": "TypeReference"
                                                                      },
                                                                      "segment": "Maybe"
                                                                  }
                                                              ],
                                                              "tag": "Link"
                                                          },
                                                          "tag": "Special"
                                                      }
                                                  ],
                                                  "tag": "Paragraph"
                                              },
                                              {
                                                  "contents": [
                                                      {
                                                          "contents": "Term",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "source:",
                                                          "tag": "Word"
                                                      }
                                                  ],
                                                  "tag": "Paragraph"
                                              },
                                              {
                                                  "contents": [
                                                      {
                                                          "contents": {
                                                              "contents": [
                                                                  {
                                                                      "contents": [
                                                                          "#qkhkl0n238s1eqibd1ecb8605sqj1m4hpoaag177cu572otqlaf1u28c8suuuqgljdtthsjtr07rv04np05o6oa27ml9105k7uas0t8",
                                                                          {
                                                                              "contents": [
                                                                                  [
                                                                                      {
                                                                                          "annotation": {
                                                                                              "contents": "#qkhkl0n238s1eqibd1ecb8605sqj1m4hpoaag177cu572otqlaf1u28c8suuuqgljdtthsjtr07rv04np05o6oa27ml9105k7uas0t8",
                                                                                              "tag": "TermReference"
                                                                                          },
                                                                                          "segment": "term"
                                                                                      },
                                                                                      {
                                                                                          "annotation": null,
                                                                                          "segment": " "
                                                                                      },
                                                                                      {
                                                                                          "annotation": {
                                                                                              "tag": "TypeAscriptionColon"
                                                                                          },
                                                                                          "segment": ": "
                                                                                      },
                                                                                      {
                                                                                          "annotation": {
                                                                                              "contents": "##Nat",
                                                                                              "tag": "TypeReference"
                                                                                          },
                                                                                          "segment": "Nat"
                                                                                      }
                                                                                  ],
                                                                                  [
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
                                                                                          "segment": " "
                                                                                      },
                                                                                      {
                                                                                          "annotation": {
                                                                                              "tag": "NumericLiteral"
                                                                                          },
                                                                                          "segment": "42"
                                                                                      }
                                                                                  ]
                                                                              ],
                                                                              "tag": "UserObject"
                                                                          }
                                                                      ],
                                                                      "tag": "Term"
                                                                  }
                                                              ],
                                                              "tag": "Source"
                                                          },
                                                          "tag": "Special"
                                                      }
                                                  ],
                                                  "tag": "Paragraph"
                                              },
                                              {
                                                  "contents": [
                                                      {
                                                          "contents": "Term",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "signature:",
                                                          "tag": "Word"
                                                      }
                                                  ],
                                                  "tag": "Paragraph"
                                              },
                                              {
                                                  "contents": [
                                                      {
                                                          "contents": {
                                                              "contents": [
                                                                  [
                                                                      {
                                                                          "annotation": {
                                                                              "contents": "#qkhkl0n238s1eqibd1ecb8605sqj1m4hpoaag177cu572otqlaf1u28c8suuuqgljdtthsjtr07rv04np05o6oa27ml9105k7uas0t8",
                                                                              "tag": "TermReference"
                                                                          },
                                                                          "segment": "term"
                                                                      },
                                                                      {
                                                                          "annotation": null,
                                                                          "segment": " "
                                                                      },
                                                                      {
                                                                          "annotation": {
                                                                              "tag": "TypeAscriptionColon"
                                                                          },
                                                                          "segment": ": "
                                                                      },
                                                                      {
                                                                          "annotation": {
                                                                              "contents": "##Nat",
                                                                              "tag": "TypeReference"
                                                                          },
                                                                          "segment": "Nat"
                                                                      }
                                                                  ]
                                                              ],
                                                              "tag": "Signature"
                                                          },
                                                          "tag": "Special"
                                                      }
                                                  ],
                                                  "tag": "Paragraph"
                                              },
                                              {
                                                  "contents": [
                                                      {
                                                          "contents": [
                                                              {
                                                                  "contents": "List",
                                                                  "tag": "Word"
                                                              },
                                                              {
                                                                  "contents": "item",
                                                                  "tag": "Word"
                                                              }
                                                          ],
                                                          "tag": "Paragraph"
                                                      }
                                                  ],
                                                  "tag": "BulletedList"
                                              },
                                              {
                                                  "contents": [
                                                      1,
                                                      [
                                                          {
                                                              "contents": [
                                                                  {
                                                                      "contents": "Numbered",
                                                                      "tag": "Word"
                                                                  },
                                                                  {
                                                                      "contents": "list",
                                                                      "tag": "Word"
                                                                  },
                                                                  {
                                                                      "contents": "item",
                                                                      "tag": "Word"
                                                                  }
                                                              ],
                                                              "tag": "Paragraph"
                                                          }
                                                      ]
                                                  ],
                                                  "tag": "NumberedList"
                                              },
                                              {
                                                  "contents": [
                                                      {
                                                          "contents": ">",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "Block",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "quote",
                                                          "tag": "Word"
                                                      }
                                                  ],
                                                  "tag": "Paragraph"
                                              },
                                              {
                                                  "contents": [
                                                      {
                                                          "contents": "Code",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "block",
                                                          "tag": "Word"
                                                      }
                                                  ],
                                                  "tag": "Paragraph"
                                              },
                                              {
                                                  "contents": [
                                                      {
                                                          "contents": "Inline",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "code:",
                                                          "tag": "Word"
                                                      }
                                                  ],
                                                  "tag": "Paragraph"
                                              },
                                              {
                                                  "contents": [
                                                      {
                                                          "contents": {
                                                              "contents": [
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
                                                                      "segment": "Nat.+"
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
                                                              "tag": "Example"
                                                          },
                                                          "tag": "Special"
                                                      }
                                                  ],
                                                  "tag": "Paragraph"
                                              },
                                              {
                                                  "contents": [
                                                      {
                                                          "contents": {
                                                              "contents": "\"doesn't typecheck\" + 1",
                                                              "tag": "Word"
                                                          },
                                                          "tag": "Code"
                                                      }
                                                  ],
                                                  "tag": "Paragraph"
                                              },
                                              {
                                                  "contents": [
                                                      {
                                                          "contents": [
                                                              {
                                                                  "contents": [
                                                                      {
                                                                          "contents": "Link",
                                                                          "tag": "Word"
                                                                      }
                                                                  ],
                                                                  "tag": "Paragraph"
                                                              },
                                                              {
                                                                  "contents": {
                                                                      "contents": "https://unison-lang.org",
                                                                      "tag": "Word"
                                                                  },
                                                                  "tag": "Group"
                                                              }
                                                          ],
                                                          "tag": "NamedLink"
                                                      }
                                                  ],
                                                  "tag": "Paragraph"
                                              },
                                              {
                                                  "contents": [
                                                      {
                                                          "contents": "![Image](https://share-next.unison-lang.org/static/unison-logo-circle.png)",
                                                          "tag": "Word"
                                                      }
                                                  ],
                                                  "tag": "Paragraph"
                                              },
                                              {
                                                  "contents": [
                                                      {
                                                          "contents": {
                                                              "contents": [
                                                                  {
                                                                      "contents": "Bold",
                                                                      "tag": "Word"
                                                                  }
                                                              ],
                                                              "tag": "Paragraph"
                                                          },
                                                          "tag": "Bold"
                                                      }
                                                  ],
                                                  "tag": "Paragraph"
                                              },
                                              {
                                                  "contents": [
                                                      {
                                                          "contents": {
                                                              "contents": [
                                                                  {
                                                                      "contents": "Italic",
                                                                      "tag": "Word"
                                                                  }
                                                              ],
                                                              "tag": "Paragraph"
                                                          },
                                                          "tag": "Bold"
                                                      }
                                                  ],
                                                  "tag": "Paragraph"
                                              },
                                              {
                                                  "contents": [
                                                      {
                                                          "contents": {
                                                              "contents": [
                                                                  {
                                                                      "contents": "Strikethrough",
                                                                      "tag": "Word"
                                                                  }
                                                              ],
                                                              "tag": "Paragraph"
                                                          },
                                                          "tag": "Strikethrough"
                                                      }
                                                  ],
                                                  "tag": "Paragraph"
                                              },
                                              {
                                                  "contents": [
                                                      {
                                                          "contents": "Horizontal",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "rule",
                                                          "tag": "Word"
                                                      }
                                                  ],
                                                  "tag": "Paragraph"
                                              },
                                              {
                                                  "contents": [
                                                      {
                                                          "contents": "---",
                                                          "tag": "Word"
                                                      }
                                                  ],
                                                  "tag": "Paragraph"
                                              },
                                              {
                                                  "contents": [
                                                      {
                                                          "contents": "Table",
                                                          "tag": "Word"
                                                      }
                                                  ],
                                                  "tag": "Paragraph"
                                              },
                                              {
                                                  "contents": [
                                                      {
                                                          "contents": "|",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "Header",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "1",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "|",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "Header",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "2",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "|",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "|",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "--------",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "|",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "--------",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "|",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "|",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "Cell",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "1",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "|",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "Cell",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "2",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "|",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "|",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "Cell",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "3",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "|",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "Cell",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "4",
                                                          "tag": "Word"
                                                      },
                                                      {
                                                          "contents": "|",
                                                          "tag": "Word"
                                                      }
                                                  ],
                                                  "tag": "Paragraph"
                                              },
                                              {
                                                  "contents": [
                                                      {
                                                          "contents": "Video",
                                                          "tag": "Word"
                                                      }
                                                  ],
                                                  "tag": "Paragraph"
                                              },
                                              {
                                                  "contents": [
                                                      {
                                                          "contents": {
                                                              "contents": [
                                                                  [
                                                                      {
                                                                          "mediaSourceMimeType": null,
                                                                          "mediaSourceUrl": "test.mp4"
                                                                      }
                                                                  ],
                                                                  {
                                                                      "poster": "test.png"
                                                                  }
                                                              ],
                                                              "tag": "Video"
                                                          },
                                                          "tag": "Special"
                                                      }
                                                  ],
                                                  "tag": "Paragraph"
                                              },
                                              {
                                                  "contents": [
                                                      {
                                                          "contents": "Transclusion/evaluation:",
                                                          "tag": "Word"
                                                      }
                                                  ],
                                                  "tag": "Paragraph"
                                              },
                                              {
                                                  "contents": [
                                                      {
                                                          "contents": [
                                                              {
                                                                  "contents": [
                                                                      {
                                                                          "contents": "This",
                                                                          "tag": "Word"
                                                                      },
                                                                      {
                                                                          "contents": "doc",
                                                                          "tag": "Word"
                                                                      },
                                                                      {
                                                                          "contents": "should",
                                                                          "tag": "Word"
                                                                      },
                                                                      {
                                                                          "contents": "be",
                                                                          "tag": "Word"
                                                                      },
                                                                      {
                                                                          "contents": "embedded.",
                                                                          "tag": "Word"
                                                                      }
                                                                  ],
                                                                  "tag": "Paragraph"
                                                              },
                                                              {
                                                                  "contents": [
                                                                      {
                                                                          "contents": "message",
                                                                          "tag": "Word"
                                                                      }
                                                                  ],
                                                                  "tag": "Paragraph"
                                                              }
                                                          ],
                                                          "tag": "UntitledSection"
                                                      }
                                                  ],
                                                  "tag": "Paragraph"
                                              }
                                          ]
                                      ],
                                      "tag": "Section"
                                  }
                              ]
                          ],
                          "tag": "Section"
                      }
                  ]
              ],
              "termNames": [
                  "term"
              ]
          }
      },
      "typeDefinitions": {}
  }
```

# Namespace Details Test

``` ucm :hide
scratch/main> builtins.mergeio
```

``` unison
{{ Documentation }}
nested.names.x = 42

nested.names.readme = {{
Here's a *README*!
}}
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      nested.names.readme : Doc2
      nested.names.x      : Nat
      nested.names.x.doc  : Doc2
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    nested.names.readme : Doc2
    nested.names.x      : Nat
    nested.names.x.doc  : Doc2
```

``` api
-- Should find names by suffix
GET /api/projects/scratch/branches/main/namespaces/nested.names
  {
      "fqn": "nested.names",
      "hash": "#6tnmlu9knsce0u2991u6fvcmf4v44fdf0aiqtmnq7mjj0gi5sephg3lf12iv3odr5rc7vlgq75ciborrd3625c701bdmdomia2gcm3o",
      "readme": {
          "contents": [
              {
                  "contents": "Here's",
                  "tag": "Word"
              },
              {
                  "contents": "a",
                  "tag": "Word"
              },
              {
                  "contents": {
                      "contents": [
                          {
                              "contents": {
                                  "contents": [
                                      {
                                          "contents": "README",
                                          "tag": "Word"
                                      }
                                  ],
                                  "tag": "Paragraph"
                              },
                              "tag": "Bold"
                          },
                          {
                              "contents": "!",
                              "tag": "Word"
                          }
                      ],
                      "tag": "Join"
                  },
                  "tag": "Group"
              }
          ],
          "tag": "Paragraph"
      }
  }
```

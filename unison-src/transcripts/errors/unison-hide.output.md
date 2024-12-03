### Transcript parser hidden errors

When an error is encountered in a `unison :hide` block
then the transcript parser should print the stanza
and surface a helpful message.

``` unison :hide
g 3
```

🛑

The transcript failed due to an error in the stanza above. The error is:

``` 
This looks like the start of an expression here 

    1 | g 3

but at the file top-level, I expect one of the following:

  - A binding, like g = 42 OR
                    g : Nat
                    g = 42
  - A watch expression, like > g + 1
  - An `ability` declaration, like unique ability Foo where ...
  - A `type` declaration, like structural type Optional a = None | Some a
```

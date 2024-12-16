from @pchiusano

``` ucm :hide
scratch/main> builtins.mergeio lib.builtin
```

``` unison :hide
printLine x = ()
foo = do
  printLine "hello there this is a pretty long string that doesn't fit"
  printLine "world"
  x = 1 + 1
  ()

d = {{ 
  
I've got some text here.

@source{foo} 

And now moar text. 

}}
```

Then

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> view foo

  foo : '()
  foo =
    do
      use Nat +
      printLine
        "hello there this is a pretty long string that doesn't fit"
      printLine "world"
      x = 1 + 1
      ()

scratch/main> display d

  I've got some text here.

      foo : '()
      foo =
        do
          use Nat +
          printLine
            "hello there this is a pretty long string that doesn't fit"
          printLine "world"
          x = 1 + 1
          ()

  And now moar text.
```

If all the things inside the `do` fit without breaking, then it does the soft hang:

``` unison :hide
foo = do
  printLine "hello there"
  printLine "world"
  x = 1 + 1
  ()
```

``` ucm :hide
scratch/main> update
```

``` ucm
scratch/main> view foo

  foo : '()
  foo = do
    use Nat +
    printLine "hello there"
    printLine "world"
    x = 1 + 1
    ()

scratch/main> display d

  I've got some text here.

      foo : '()
      foo = do
        use Nat +
        printLine "hello there"
        printLine "world"
        x = 1 + 1
        ()

  And now moar text.
```

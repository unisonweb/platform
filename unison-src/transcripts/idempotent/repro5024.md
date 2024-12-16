``` unison :error
fucntionA x =
  if x then
    1
  esle
    2

functionB y = y
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I got confused here:

      7 | functionB y = y


  I was surprised to find a = here.
  I was expecting one of these instead:

  * and
  * bang
  * do
  * false
  * force
  * handle
  * if
  * infixApp
  * let
  * newline or semicolon
  * or
  * quote
  * termLink
  * true
  * tuple
  * typeLink
```

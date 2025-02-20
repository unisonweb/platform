Just a bunch of random parse errors to test the error formatting.

``` unison :error
x =
  foo.123
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I got confused here:

      2 |   foo.123


  I was surprised to find a 1 here.
  I was expecting one of these instead:

  * end of input
  * hash (ex: #af3sj3)
  * identifier (ex: abba1, snake_case, .foo.bar#xyz, .foo.++#xyz, or 🌻)
```

``` unison :error
namespace.blah = 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I got confused here:

      1 | namespace.blah = 1


  I was surprised to find a .blah here.
```

``` unison :error
x = 1 ]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found a closing ']' here without a matching '['.

      1 | x = 1 ]
```

``` unison :error
x = a.#abc
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I got confused here:

      1 | x = a.#abc


  I was surprised to find a '.' here.
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

``` unison :error
x = "hi
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I got confused here:

      2 | 

  I was surprised to find an end of input here.
  I was expecting one of these instead:

  * "
  * \s
  * literal character
```

``` unison :error
y : a
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I got confused here:

      2 | 

  I was surprised to find an end of section here.
  I was expecting one of these instead:

  * ->
  * newline or semicolon
```

# Test that the options selector for fuzzy finding is working as expected for different argument types.

If an argument is required but doesn't have a fuzzy resolver, the command should just print the help.

``` ucm :error
-- The second argument of move.term is a 'new-name' and doesn't have a fuzzy resolver

scratch/main> move.term

  `move.term foo bar` renames `foo` to `bar`.
```

If a fuzzy resolver doesn't have any options available it should print a message instead of
opening an empty fuzzy-select.

``` ucm :error
scratch/empty> view

  ⚠️

  Sorry, I was expecting an argument for the definition to view, and I couldn't find any to suggest to you. 😅
```

``` unison :hide
optionOne = 1

nested.optionTwo = 2
```

Definition args

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    nested.optionTwo : ##Nat
    optionOne        : ##Nat

scratch/main> debug.fuzzy-options view _

  Select a definition to view:
    * optionOne
    * nested.optionTwo
```

Namespace args

``` ucm
scratch/main> add

  ⊡ Ignored previously added definitions: nested.optionTwo
    optionOne

scratch/main> debug.fuzzy-options find-in _

  Select a namespace:
    * nested
```

Project Branch args

``` ucm
myproject/main> branch mybranch

  Done. I've created the mybranch branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /mybranch`.

scratch/main> debug.fuzzy-options switch _

  Select a project or branch to switch to:
    * /empty
    * /main
    * myproject/main
    * myproject/mybranch
    * scratch/empty
    * scratch/main
    * myproject
    * scratch
```

Ensure that Records keep their syntax after being added to the codebase

``` ucm :hide
scratch/main> builtins.merge

scratch/main> load "unison-src/transcripts-using-base/base.u"
```

## Record with 1 field

``` unison :hide
unique type Record1 = { a : Text }
```

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> view Record1

  type Record1 = { a : Text }
```

## Record with 2 fields

``` unison :hide
unique type Record2 = { a : Text, b : Int }
```

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> view Record2

  type Record2 = { a : Text, b : Int }
```

## Record with 3 fields

``` unison :hide
unique type Record3 = { a : Text, b : Int, c : Nat }
```

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> view Record3

  type Record3 = { a : Text, b : Int, c : Nat }
```

## Record with many fields

``` unison :hide
unique type Record4 =
  { a : Text
  , b : Int
  , c : Nat
  , d : Bytes
  , e : Text
  , f : Nat
  , g : [Nat]
  }
```

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> view Record4

  type Record4
    = { a : Text,
        b : Int,
        c : Nat,
        d : Bytes,
        e : Text,
        f : Nat,
        g : [Nat] }
```

## Record with many many fields

``` unison :hide
unique type Record5 = {
  zero : Nat,
  one : [Nat],
  two : [[Nat]],
  three: [[[Nat]]],
  four: [[[[Nat]]]],
  five: [[[[[Nat]]]]],
  six: [[[[[[Nat]]]]]],
  seven: [[[[[[[Nat]]]]]]],
  eight: [[[[[[[[Nat]]]]]]]],
  nine: [[[[[[[[[Nat]]]]]]]]],
  ten: [[[[[[[[[[Nat]]]]]]]]]],
  eleven: [[[[[[[[[[[Nat]]]]]]]]]]],
  twelve: [[[[[[[[[[[[Nat]]]]]]]]]]]],
  thirteen: [[[[[[[[[[[[[Nat]]]]]]]]]]]]],
  fourteen: [[[[[[[[[[[[[[Nat]]]]]]]]]]]]]],
  fifteen: [[[[[[[[[[[[[[[Nat]]]]]]]]]]]]]]],
  sixteen: [[[[[[[[[[[[[[[[Nat]]]]]]]]]]]]]]]],
  seventeen: [[[[[[[[[[[[[[[[[Nat]]]]]]]]]]]]]]]]],
  eighteen: [[[[[[[[[[[[[[[[[[Nat]]]]]]]]]]]]]]]]]],
  nineteen: [[[[[[[[[[[[[[[[[[[Nat]]]]]]]]]]]]]]]]]]],
  twenty: [[[[[[[[[[[[[[[[[[[[Nat]]]]]]]]]]]]]]]]]]]]
}
```

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> view Record5

  type Record5
    = { zero : Nat,
        one : [Nat],
        two : [[Nat]],
        three : [[[Nat]]],
        four : [[[[Nat]]]],
        five : [[[[[Nat]]]]],
        six : [[[[[[Nat]]]]]],
        seven : [[[[[[[Nat]]]]]]],
        eight : [[[[[[[[Nat]]]]]]]],
        nine : [[[[[[[[[Nat]]]]]]]]],
        ten : [[[[[[[[[[Nat]]]]]]]]]],
        eleven : [[[[[[[[[[[Nat]]]]]]]]]]],
        twelve : [[[[[[[[[[[[Nat]]]]]]]]]]]],
        thirteen : [[[[[[[[[[[[[Nat]]]]]]]]]]]]],
        fourteen : [[[[[[[[[[[[[[Nat]]]]]]]]]]]]]],
        fifteen : [[[[[[[[[[[[[[[Nat]]]]]]]]]]]]]]],
        sixteen : [[[[[[[[[[[[[[[[Nat]]]]]]]]]]]]]]]],
        seventeen : [[[[[[[[[[[[[[[[[Nat]]]]]]]]]]]]]]]]],
        eighteen : [[[[[[[[[[[[[[[[[[Nat]]]]]]]]]]]]]]]]]],
        nineteen : [[[[[[[[[[[[[[[[[[[Nat]]]]]]]]]]]]]]]]]]],
        twenty : [[[[[[[[[[[[[[[[[[[[Nat]]]]]]]]]]]]]]]]]]]] }
```

## Record with user-defined type fields

This record type has two fields whose types are user-defined (`Record4` and `UserType`).

``` unison :hide
unique type UserType = UserType Nat

unique type RecordWithUserType = { a : Text, b : Record4, c : UserType }
```

``` ucm :hide
scratch/main> add
```

If you `view` or `edit` it, it *should* be treated as a record type, but it does not (which is a bug)

``` ucm
scratch/main> view RecordWithUserType

  type RecordWithUserType
    = { a : Text, b : Record4, c : UserType }
```

## Syntax

Trailing commas are allowed.

``` unison
unique type Record5 =
  { a : Text,
    b : Int,
  }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      Record5.a        : Record5 -> Text
      Record5.a.modify : (Text ->{g} Text)
                         -> Record5
                         ->{g} Record5
      Record5.a.set    : Text -> Record5 -> Record5
      Record5.b        : Record5 -> Int
      Record5.b.modify : (Int ->{g} Int)
                         -> Record5
                         ->{g} Record5
      Record5.b.set    : Int -> Record5 -> Record5
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      type Record5
```

# No Hashes in Term Declarations

There should not be hashes in the names used in term declarations, either in the type signature or the type definition.

``` unison :hide:all:error
x##Nat : Int -> Int -> Boolean
x##Nat = 5
```

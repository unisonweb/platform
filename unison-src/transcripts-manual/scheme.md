This transcript executes very slowly, because the compiler has an
entire copy of base (and other stuff) within it.

``` ucm :hide
scratch/main> builtins.merge
scratch/main> pull.without-history unison.public.base.trunk base
```

``` unison
stdOut = stdHandle StdOut

print txt =
  match putBytes.impl stdOut (toUtf8 txt) with
    Left f -> raise f
    Right _ -> ()

prints n f = print (join " " f n ++ "\n")

join sep f =
  use Text ++
  loop acc = cases
    0 -> acc
    n -> loop (!f ++ (sep ++ acc)) (drop n 1)

  cases
    0 -> ""
    n -> loop !f (drop n 1)

addUp : Nat -> Nat -> Nat
addUp acc = cases
  0 -> acc
  n -> addUp (1+acc) (drop n 1)

repeat : Nat -> '{g} () -> '{g} ()
repeat n act =
  loop : Nat ->{g} ()
  loop = cases
    0 -> ()
    k ->
      !act
      loop (drop k 1)

  '(loop n)

printAddUp : Nat ->{IO,Exception} ()
printAddUp n =
  ns = [addUp 0 n, addUp 0 n, addUp 0 n, addUp 0 n, addUp 0 n]
  prints 8 '(toText (addUp 0 n))

singleAddUp : '{IO,Exception} ()
singleAddUp = do printAddUp 3000000

multiAddUp : '{IO,Exception} ()
multiAddUp = repeat 35 '(printAddUp 3000000)
```

``` ucm
scratch/main> add
scratch/main> run singleAddUp
scratch/main> run.native multiAddUp
```

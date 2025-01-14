``` unison

mutual0 = cases
  0 -> "okay"
  n ->
    _ = openFile
    mutual1 (drop n 1)

mutual1 n =
  mutual0 n

mkTestCase = do
  saveTestCase "case-04" "v4" mutual1 5
```

``` ucm
scratch/main> add
scratch/main> run mkTestCase
```

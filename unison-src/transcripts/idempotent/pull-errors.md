``` ucm :error
test/main> pull "@aryairani/test-almost-empty/main" lib.base_latest

  The use of `pull` to install libraries is now deprecated.
  Going forward, you can use
  `lib.install @aryairani/test-almost-empty/main`.

  Downloaded 2 entities.

  I installed @aryairani/test-almost-empty/main as
  aryairani_test_almost_empty_main.

test/main> pull "@aryairani/test-almost-empty/main" a.b

  ⚠️

  Sorry, I wasn’t sure how to process your request:

    I think you want to merge @aryairani/test-almost-empty/main
    into the a.b namespace, but the `pull` command only supports
    merging into the top level of a local project branch.

  You can run `help pull` for more information on using `pull`.

test/main> pull "@aryairani/test-almost-empty/main" a

  I think you want to merge @aryairani/test-almost-empty/main
  into the a branch, but it doesn't exist. If you want, you can
  create it with `branch.empty a`, and then `pull` again.

test/main> pull "@aryairani/test-almost-empty/main" .a

  ⚠️

  Sorry, I wasn’t sure how to process your request:

    I think you want to merge @aryairani/test-almost-empty/main
    into the .a namespace, but the `pull` command only supports
    merging into the top level of a local project branch.

  You can run `help pull` for more information on using `pull`.
```

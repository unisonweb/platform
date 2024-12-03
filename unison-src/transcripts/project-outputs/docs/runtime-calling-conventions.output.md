### Lambda

  - called with arguments in declared order.
  - whenever a lambda is called, it takes itself as `rec`
  - it evaluates the body passing the bound lambda parameters on the stack

### Computation

  - bound variables passed on stack with innermost scope closest to index 0
  - "rec" is passed as well (?)
  - evaluations in nontail positions need to catch handle TC because their "frame"
    has more work and shouldn't be thrown away; evaluations in tail positions can
    throw their tailcalls upward and discard their frame
  - let1/letrec evaluate the bindings with the existing stack (bound variables in their scope);
    body is called with bindings prepended to stack
  - compilevar returns rec if its name matches currentRec,
    otherwise looks up a value on the bindings stack
  - compilelambda returns a computation that will produce a lambda when evaluated
  - apply
      - if fn name matches currentRec, then staticRecCall
          - staticRecTailCall
              - throw selfTailCall with evaluated args (seems like this would not do anything) (?)
          - staticRecNonTailCall
              - call (rec: Lambda) with evaluated args
              - a SelfCall exception should never escape the wrapper lambda
      - else compile fn
          - if compiled fn is Return(Lambda)
              - staticTailCall
                  - throw tailcall with fn
              - staticNonTailCall
                  - call fn with rec = fn
          - else compiled fn is not yet a lambda, and needs to be evaluated again (at least once)
              - dynamicTailCall
                  - eval mkFn and assume it produces a lambda (it should)
                  - throw tailcall with lambda and eval'd args
              - dynamicNontailCall
                  - eval mkFn and assume it produces a lambda (it should\!)
                  - call lambda with evaluated args

### Tail calls

tailcall throws an exception with the target function & args
selfTailCall throws a tailcall with null(implied?) function

when a tailcall exception is caught, we enter a while loop which calls the
target function and continues to catch tail calls until the target function
is null.

note that the selftailcall begins with null. (?) don't understand

### annotated bounds

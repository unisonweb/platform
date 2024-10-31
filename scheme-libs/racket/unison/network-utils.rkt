#lang racket/base
(require racket/exn
         unison/data ; exception
         unison/data-info ; ref-*
         unison/chunked-seq
         unison/core) ; exception->string, chunked-string

(provide
  handle-errors
  (struct-out socket-pair))

(struct socket-pair (input output))

(define-syntax handle-errors
  (syntax-rules ()
    [(_ ex ...)
     (with-handlers
       [[exn:fail:network?
          (lambda (e)
            (exception
              ref-iofailure:typelink
              (exception->string e)
              ref-unit-unit))]
        [exn:fail:contract?
          (lambda (e)
            (exception
              ref-miscfailure:typelink
              (exception->string e)
              ref-unit-unit))]
        [(lambda _ #t)
         (lambda (e)
           (exception
             ref-miscfailure:typelink
             (string->chunked-string
               (format "Unknown exception ~a" (exn->string e)))
             ref-unit-unit))]]
       ex ...)]))


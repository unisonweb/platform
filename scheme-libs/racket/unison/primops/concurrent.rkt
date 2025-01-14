
#lang racket/base

(require unison/boot
         unison/concurrent
         unison/data
         unison/data-info)

(provide
  builtin-IO.delay.impl.v3
  builtin-IO.delay.impl.v3:termlink
  builtin-IO.forkComp.v2
  builtin-IO.forkComp.v2:termlink
  builtin-IO.kill.impl.v3
  builtin-IO.kill.impl.v3:termlink

  builtin-Promise.new
  builtin-Promise.new:termlink
  builtin-Promise.read
  builtin-Promise.read:termlink
  builtin-Promise.tryRead
  builtin-Promise.tryRead:termlink
  builtin-Promise.write
  builtin-Promise.write:termlink
  builtin-ThreadId.toText
  builtin-ThreadId.toText:termlink)


(define-unison-builtin (builtin-Promise.new _) (promise-new))

(define-unison-builtin (builtin-Promise.read p) (promise-read p))

(define-unison-builtin (builtin-Promise.tryRead p) (promise-try-read p))

(define-unison-builtin (builtin-Promise.write p v) (promise-write p v))

(define-unison-builtin (builtin-ThreadId.toText tid)
  (string->chunked-string (describe-value tid)))

(define-unison-builtin (builtin-IO.delay.impl.v3 micros)
  ; TODO: this seems like it should have error handling, but it hadn't
  ; been implemented yet.
  (sleep micros)
  (ref-either-right ref-unit-unit))

(define-unison-builtin (builtin-IO.forkComp.v2 k)
  (fork (lambda () (k ref-unit-unit))))

(define-unison-builtin (builtin-IO.kill.impl.v3 tid)
  ; TODO: this seems like it should have error handling, but it hadn't
  ; been implemented yet.
  (kill tid)
  (ref-either-right ref-unit-unit))

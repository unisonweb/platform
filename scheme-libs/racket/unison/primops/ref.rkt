#lang racket/base

(require unison/boot
         unison/concurrent
         unison/data
         unison/data-info)

(provide
  builtin-IO.ref
  builtin-IO.ref:termlink
  builtin-Ref.Ticket.read
  builtin-Ref.Ticket.read:termlink
  builtin-Ref.cas
  builtin-Ref.cas:termlink
  builtin-Ref.read
  builtin-Ref.read:termlink
  builtin-Ref.readForCas
  builtin-Ref.readForCas:termlink
  builtin-Ref.write
  builtin-Ref.write:termlink
  builtin-Scope.ref
  builtin-Scope.ref:termlink)


(define-unison-builtin (builtin-IO.ref v)
  (ref-new v))

(define-unison-builtin (builtin-Ref.Ticket.read r) r)

(define-unison-builtin (builtin-Ref.cas ref ticket value)
  (ref-cas ref ticket value))

(define-unison-builtin (builtin-Ref.read r)
  (ref-read r))

(define-unison-builtin (builtin-Ref.readForCas r)
  (ref-read r))

(define-unison-builtin (builtin-Ref.write r v)
  (ref-write r v)
  ref-unit-unit)

(define-unison-builtin (builtin-Scope.ref v)
  (ref-new v))

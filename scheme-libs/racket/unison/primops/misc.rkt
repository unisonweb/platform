
#lang racket/base

(require unison/boot
         unison/chunked-seq
         unison/data
         unison/data-info
         unison/murmurhash)

(require racket/match)

(provide
  builtin-Boolean.not
  builtin-Boolean.not:termlink

  builtin-Any.Any
  builtin-Any.Any:termlink
  builtin-Any.unsafeExtract
  builtin-Any.unsafeExtract:termlink

  builtin-Debug.toText
  builtin-Debug.toText:termlink
  builtin-Debug.trace
  builtin-Debug.trace:termlink
  builtin-Debug.watch
  builtin-Debug.watch:termlink

  builtin-Scope.run
  builtin-Scope.run:termlink

  builtin-bug
  builtin-bug:termlink

  builtin-Universal.murmurHash:termlink

  builtin-unsafe.coerceAbilities
  builtin-unsafe.coerceAbilities:termlink

  builtin-jumpCont
  builtin-jumpCont:termlink
  builtin-todo
  builtin-todo:termlink

  builtin-Link.Term.toText
  builtin-Link.Term.toText:termlink

  builtin-Value.toBuiltin
  builtin-Value.toBuiltin:termlink
  builtin-Value.fromBuiltin
  builtin-Value.fromBuiltin:termlink
  builtin-Code.fromGroup
  builtin-Code.fromGroup:termlink
  builtin-Code.toGroup
  builtin-Code.toGroup:termlink
  builtin-TermLink.fromReferent
  builtin-TermLink.fromReferent:termlink
  builtin-TermLink.toReferent
  builtin-TermLink.toReferent:termlink
  builtin-TypeLink.toReference
  builtin-TypeLink.toReference:termlink


  ; fake builtins
  builtin-murmurHashBytes)



(define-unison-builtin (builtin-Boolean.not b) (not b))

(define-unison-builtin (builtin-Any.Any x) (unison-any-any x))

(define-unison-builtin (builtin-Any.unsafeExtract x)
  (match x
    [(unison-data r t (list x)) x]))

(define-unison-builtin (builtin-Debug.toText v)
  (ref-optional-some
    (ref-either-left
      (string->chunked-string
        (describe-value v)))))

(define-unison-builtin (builtin-Debug.trace msg v)
  (display "trace: ")
  (displayln (chunked-string->string msg))
  (displayln (describe-value v))
  ref-unit-unit)

(define-unison-builtin (builtin-Debug.watch msg v)
  (displayln (chunked-string->string msg))
  v)

(define-unison-builtin (builtin-bug x)
  (raise (make-exn:bug "builtin.bug" x)))

(define-unison-builtin (builtin-jumpCont k v) (k v))

(define-unison-builtin (builtin-todo x)
  (raise (make-exn:bug "builtin.todo" x)))

(define-unison-builtin (builtin-Scope.run k)
  (k ref-unit-unit))

(define-builtin-link Universal.murmurHash)

(define-unison-builtin (builtin-murmurHashBytes bs)
  (murmurhash-bytes (chunked-bytes->bytes bs)))

(define-unison-builtin (builtin-unsafe.coerceAbilities f) f)

(define-unison-builtin (builtin-Link.Term.toText ln)
  (string->chunked-string (termlink->string ln)))

(define-unison-builtin (builtin-Value.toBuiltin v) (unison-quote v))
(define-unison-builtin (builtin-Value.fromBuiltin v)
  (unison-quote-val v))
(define-unison-builtin (builtin-Code.fromGroup sg) (unison-code sg))
(define-unison-builtin (builtin-Code.toGroup co)
  (unison-code-rep co))
(define-unison-builtin (builtin-TermLink.fromReferent rf)
  (referent->termlink rf))
(define-unison-builtin (builtin-TermLink.toReferent tl)
  (termlink->referent tl))
(define-unison-builtin (builtin-TypeLink.toReference tl)
  (typelink->reference tl))

(define-unison-builtin (builtin-Link.Type.toText ln)
  (string->chunked-string (typelink->string ln)))


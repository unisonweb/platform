
#lang racket/base

(require unison/boot
         unison/chunked-seq
         unison/data
         unison/data-info)

(provide
  builtin-List.++
  builtin-List.++:termlink
  builtin-List.at
  builtin-List.at:termlink
  builtin-List.cons
  builtin-List.cons:termlink
  builtin-List.drop
  builtin-List.drop:termlink
  builtin-List.size
  builtin-List.size:termlink
  builtin-List.snoc
  builtin-List.snoc:termlink
  builtin-List.splitLeft
  builtin-List.splitLeft:termlink
  builtin-List.splitRight
  builtin-List.splitRight:termlink
  builtin-List.take
  builtin-List.take:termlink
  builtin-List.viewl
  builtin-List.viewl:termlink
  builtin-List.viewr
  builtin-List.viewr:termlink)


(define-unison-builtin (builtin-List.++ xs ys)
  (chunked-list-append xs ys))

(define-unison-builtin (builtin-List.at n xs)
  (with-handlers
    ([exn:fail:contract? (lambda (e) ref-optional-none)])
    (ref-optional-some (chunked-list-ref xs n))))

(define-unison-builtin (builtin-List.cons x xs)
  (chunked-list-add-first xs x))

(define-unison-builtin (builtin-List.drop n xs)
  (chunked-list-drop xs n))

(define-unison-builtin (builtin-List.size xs)
  (chunked-list-length xs))

(define-unison-builtin (builtin-List.snoc xs x)
  (chunked-list-add-last xs x))

(define-unison-builtin (builtin-List.take n xs)
  (chunked-list-take xs n))

(define-unison-builtin (builtin-List.viewl xs)
  (if (chunked-list-empty? xs)
    ref-seqview-empty
    (let-values ([(t h) (chunked-list-pop-first xs)])
      (ref-seqview-elem h t))))

(define-unison-builtin (builtin-List.viewr xs)
  (if (chunked-list-empty? xs)
    ref-seqview-empty
    (let-values ([(t h) (chunked-list-pop-last xs)])
      (ref-seqview-elem t h))))

(define-unison-builtin (builtin-List.splitLeft n s)
  (if (< (chunked-list-length s) n)
    ref-seqview-empty
    (let-values ([(l r) (chunked-list-split-at s n)])
      (ref-seqview-elem l r))))

; Copied TODO: write test that stresses this
(define-unison-builtin (builtin-List.splitRight n s)
  (define len (chunked-list-length s))

  (if (< len n)
    ref-seqview-empty
    (let-values ([(l r) (chunked-list-split-at s (- len n))])
      (ref-seqview-elem l r))))


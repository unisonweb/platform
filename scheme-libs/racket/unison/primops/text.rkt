
#lang racket/base

(require unison/boot
         unison/chunked-seq
         (only-in unison/core
                  chunked-string-foldMap-chunks
                  chunked-string<?)
         unison/data
         unison/data-info
         unison/string-search)

(require (only-in srfi/13 string-reverse))

(provide
  builtin-Char.fromNat
  builtin-Char.fromNat:termlink
  builtin-Char.toNat
  builtin-Char.toNat:termlink
  builtin-Char.toText
  builtin-Char.toText:termlink

  builtin-Text.indexOf
  builtin-Text.indexOf:termlink
  builtin-Text.==
  builtin-Text.==:termlink
  builtin-Text.!=
  builtin-Text.!=:termlink
  builtin-Text.<=
  builtin-Text.<=:termlink
  builtin-Text.>=
  builtin-Text.>=:termlink
  builtin-Text.<
  builtin-Text.<:termlink
  builtin-Text.>
  builtin-Text.>:termlink
  builtin-Text.++
  builtin-Text.++:termlink
  builtin-Text.drop
  builtin-Text.drop:termlink
  builtin-Text.empty
  builtin-Text.empty:termlink
  builtin-Text.fromCharList
  builtin-Text.fromCharList:termlink
  builtin-Text.fromUtf8.impl.v3
  builtin-Text.fromUtf8.impl.v3:termlink
  builtin-Text.repeat
  builtin-Text.repeat:termlink
  builtin-Text.reverse
  builtin-Text.reverse:termlink
  builtin-Text.size
  builtin-Text.size:termlink
  builtin-Text.take
  builtin-Text.take:termlink
  builtin-Text.toCharList
  builtin-Text.toCharList:termlink
  builtin-Text.toLowercase
  builtin-Text.toLowercase:termlink
  builtin-Text.toUppercase
  builtin-Text.toUppercase:termlink
  builtin-Text.toUtf8
  builtin-Text.toUtf8:termlink
  builtin-Text.uncons
  builtin-Text.uncons:termlink
  builtin-Text.unsnoc
  builtin-Text.unsnoc:termlink)


(define-unison-builtin (builtin-Char.fromNat n)
  (integer->char n))

(define-unison-builtin (builtin-Char.toNat c)
  (char->integer c))

(define-unison-builtin (builtin-Char.toText c)
  (string->chunked-string (string c)))

(define-unison-builtin (builtin-Text.repeat n t)
  (let loop ([i 0]
             [acc empty-chunked-string])
    (if (= i n)
      acc
      (loop (add1 i) (chunked-string-append acc t)))))

(define-unison-builtin (builtin-Text.reverse t)
  (chunked-string-foldMap-chunks
    t
    string-reverse
    (lambda (acc c) (chunked-string-append c acc))))

(define-unison-builtin (builtin-Text.size t) (chunked-string-length t))

(define-unison-builtin (builtin-Text.take n t) (chunked-string-take t n))

(define-unison-builtin (builtin-Text.toCharList t)
  (build-chunked-list
    (chunked-string-length t)
    (lambda (i) (chunked-string-ref t i))))

(define-unison-builtin (builtin-Text.toLowercase t)
  (chunked-string-foldMap-chunks t string-downcase chunked-string-append))

(define-unison-builtin (builtin-Text.toUppercase t)
  (chunked-string-foldMap-chunks t string-upcase chunked-string-append))

(define-unison-builtin (builtin-Text.toUtf8 t)
  (bytes->chunked-bytes
    (string->bytes/utf-8
      (chunked-string->string t))))

(define-unison-builtin (builtin-Text.uncons s)
  (cond
    [(chunked-string-empty? s) ref-optional-none]
    [else
     (let-values ([(t c) (chunked-string-pop-first s)])
       (ref-optional-some (unison-tuple c t)))]))

(define-unison-builtin (builtin-Text.unsnoc s)
  (cond
    [(chunked-string-empty? s) ref-optional-none]
    [else
     (let-values ([(t c) (chunked-string-pop-last s)])
       (ref-optional-some (unison-tuple t c)))]))

; Note: chunked-string<? is actually <=
(define-unison-builtin (builtin-Text.> x y)
  (not (chunked-string<? x y)))

(define-unison-builtin (builtin-Text.< x y)
  (not (chunked-string<? y x)))

(define-unison-builtin (builtin-Text.>= x y) (chunked-string<? y x))

(define-unison-builtin (builtin-Text.<= x y) (chunked-string<? x y))

(define-unison-builtin (builtin-Text.== x y) (equal? x y))

(define-unison-builtin (builtin-Text.!= x y) (not (equal? x y)))

(define (->optional v)
  (if v
      (ref-optional-some v)
      ref-optional-none))

(define-unison-builtin (builtin-Text.indexOf n h)
  (->optional (chunked-string-index-of h n)))

(define-unison-builtin (builtin-Text.++ t u)
  (chunked-string-append t u))

(define-unison-builtin (builtin-Text.drop n t)
  (chunked-string-drop t n))

(define-unison-builtin #:hints [value] (builtin-Text.empty)
  empty-chunked-string)

(define-unison-builtin (builtin-Text.fromCharList cs)
  (build-chunked-string
    (chunked-list-length cs)
    (lambda (i) (chunked-list-ref cs i))))

(define-unison-builtin (builtin-Text.fromUtf8.impl.v3 bs)
  (with-handlers
    ([exn:fail:contract?
       (lambda (e)
         (ref-either-left
           (ref-failure-failure
             ref-iofailure:typelink
             (string->chunked-string
               (string-append
                 "Invalid UTF-8 stream: "
                 (describe-value bs)))
             (unison-any-any (exception->string e)))))])
    (ref-either-right
      (string->chunked-string
        (bytes->string/utf-8
          (chunked-bytes->bytes bs))))))


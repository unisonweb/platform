
#lang racket

(provide
  unison-curry
  unison-curry-0
  unison-curry-1
  unison-curry-2
  unison-curry-3
  unison-curry-4
  unison-curry-5
  unison-curry-6
  unison-curry-7
  unison-curry-8
  unison-curry-9)

(require racket/performance-hint
         racket/unsafe/undefined
         (for-syntax
           (only-in racket
                    const range match empty-sequence))
         unison/data)

(define-for-syntax (vsym #:pre [pre "x"] n)
  (string->symbol (string-append pre (number->string n))))

(define-for-syntax (curry-cases loc n ref:stx fun:stx us vs)
  (define (sub us vs) (curry-expr loc n ref:stx fun:stx us vs))

  (for/foldr ([cases (list)]) ([p (in-partitions vs)])
    (match p
      [(cons pre post)
       (with-syntax ([(u ...) us]
                     [(v ...) pre]
                     [f fun:stx])
         (cond
           [(null? post)
            (list*
              (syntax/loc loc
                [(v ...) (f u ... v ...)])
              (syntax/loc loc
                [(v ... . rest) (apply (f u ... v ...) rest)])
              cases)]
           [else
            (with-syntax ([sc (sub (append us pre) post)])
              (cons
                (syntax/loc loc [(v ...) sc])
                cases))]))])))

; Build case-lambdas that are nested n-deep for partitions of
; variables us and vs.
(define-for-syntax (curry-expr loc n ref:stx fun:stx us vs)
  (cond
    [(= 0 n)
     (with-syntax ([(u ...) us] [gr ref:stx] [f fun:stx])
       (syntax/loc loc
         (unison-closure gr f (list u ...))))]
    [else
     (with-syntax ([(c ...) (curry-cases loc (sub1 n) ref:stx fun:stx us vs)])
       (syntax/loc loc
         (case-lambda c ...)))]))

(define-for-syntax (in-parts pre post)
  (in-sequences
    (in-value (cons (reverse pre) post))
    (match post
      ['() empty-sequence]
      [(cons x xs) (in-parts (cons x pre) xs)])))

(define-for-syntax (in-partitions xs) (in-parts '() xs))

(define-for-syntax (build-curried loc n ref:stx fun:stx)
  (define xs:stx (generate-temporaries (map (const 'x) (range n))))

  (curry-expr loc 2 ref:stx fun:stx '() xs:stx))

(define-for-syntax (build-curry loc n)
  (define ref:stx (syntax/loc loc gr))
  (define fun:stx (syntax/loc loc f))

  (with-syntax ([body (build-curried loc n ref:stx fun:stx)])
    (syntax/loc loc
      (lambda (gr f) body))))

(define-syntax (make-curry stx)
  (syntax-case stx ()
    [(make-curry n gr f)
     (build-curried stx (syntax->datum #'n) #'gr #'f)]))
     ; (build-curry stx (syntax->datum #'n))]))

(begin-encourage-inline
  (define ((unison-curry-0 gr f) #:reflect [ref? unsafe-undefined] . rest)
    (if (eq? ref? unsafe-undefined)
      (if (= (length rest) 0)
        (f)
        (apply (f) rest))
      (unison-closure gr f rest)))

  (define (unison-curry-1 gr f) (make-curry 1 gr f))
  (define (unison-curry-2 gr f) (make-curry 2 gr f))
  (define (unison-curry-3 gr f) (make-curry 3 gr f))
  (define (unison-curry-4 gr f) (make-curry 4 gr f))
  (define (unison-curry-5 gr f) (make-curry 5 gr f))
  (define (unison-curry-6 gr f) (make-curry 6 gr f))
  (define (unison-curry-7 gr f) (make-curry 7 gr f))
  (define (unison-curry-8 gr f) (make-curry 8 gr f))
  (define (unison-curry-9 gr f) (make-curry 9 gr f)))

(define-syntax (unison-curry stx)
  (syntax-case stx ()
    [(unison-curry #:inline n gr f)
     (build-curried stx (syntax->datum #'n) #'gr #'f)]
    [(unison-curry n gr f)
     (let ([m (syntax->datum #'n)])
       (cond
         [(< m 10)
          (define curry:stx (vsym #:pre "unison-curry-" m))
          (with-syntax ([u-curry curry:stx])
            (syntax/loc stx
              (u-curry gr f)))]
         [else
          (build-curried stx m #'gr #'f)]))]))



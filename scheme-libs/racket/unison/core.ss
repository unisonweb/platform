; This library implements various functions and macros that are used
; internally to the unison scheme libraries. This provides e.g. a
; measure of abstraction for the particular scheme platform. A useful
; feature of one implementation might need to be implemented on top of
; other features of another, and would go in this library.
;
; This library won't be directly imported by the generated unison
; code, so if some function is needed for those, it should be
; re-exported by (unison boot).
#!racket/base
(provide
  universal-compare
  chunked-string<?
  universal=?

  fx1-

  ; syntax->list
  (for-syntax raise-syntax-error)

  exception->string

  (struct-out exn:bug)

  let-marks
  call-with-marks
  ref-mark

  chunked-string-foldMap-chunks

  unison-tuple
  list->unison-tuple

  bytevector
  bytevector-append

  decode-value
  describe-value
  describe-hash

  bytevector->string/utf-8
  string->bytevector/utf-8)

(require
  (rename-in (only-in racket
                current-inexact-milliseconds
                directory-list
                string-copy!
                bytes
                bytes-append
                bytes?
                bytes=?
                bytes<?
                bytes->string/utf-8
                string->bytes/utf-8
                with-continuation-mark
                continuation-mark-set-first
                raise-syntax-error
                build-path
                path->string
                match
                match*
                string-append*
                for/fold)
          (string-copy! racket-string-copy!)
          (bytes-append bytevector-append)
          (string->bytes/utf-8 string->bytevector/utf-8)
          (bytes->string/utf-8 bytevector->string/utf-8)
          (bytes bytevector))
  (only-in rnrs/base-6 div mod div-and-mod)
  racket/exn
  (only-in racket/fixnum fl->fx fx- fxand fxlshift fxrshift fxior)
  racket/unsafe/ops
  unison/bytevector
  unison/data
  unison/data-info
  unison/chunked-seq)

(define (fx1- n) (fx- n 1))

(define (decode-value x) '())

; 48 = #\0
; 87 = #\a - 10
(define (bytevector->base16-string bs)
  (define (b16 n) (integer->char (+ n (if (< n 10) 48 87))))

  (let* ([ilen (bytes-length bs)]
         [out (make-string (* 2 ilen))])
    (let rec ([i 0] [o 0])
      (cond
        [(>= i ilen) out]
        [else
          (let-values
            ([(c0 c1) (div-and-mod (bytes-ref bs i) 16)])
            (string-set! out o       (b16 c0))
            (string-set! out (+ o 1) (b16 c1))
            (rec (+ i 1) (+ o 2)))]))))


(define (describe-list op cl l)
  (let rec ([pre (string op)] [post (string op cl)] [cur l])
    (cond
      [(null? cur) post]
      [else
        (let* ([sx (describe-value (car cur))]
               [sxs (rec ", " (string cl) (cdr cur))])
          (string-append pre sx sxs))])))

(define (describe-list-sq l) (describe-list #\[ #\] l))
(define (describe-list-br l) (describe-list #\{ #\} l))

(define (describe-hash h)
  (substring (bytevector->base32-string h #:alphabet 'hex) 0 8))

(define (describe-derived h i)
  (let ([th (describe-hash h)]
        [ti (if (= i 0) "" (string-append "." (number->string i)))])
    (string-append "#" th ti)))

(define (describe-ref r)
  (match r
    [(? symbol?) (symbol->string r)]
    [(unison-data r 0 (list name)) (string-append "##" name)]
    [(unison-data r 1 (list (unison-data s 0 (list bs ix))))
     (describe-derived bs ix)]
    [(unison-typelink-builtin name) (string-append "##" name)]
    [(unison-typelink-derived hash i) (describe-derived hash i)]
    [(unison-termlink-builtin name) (string-append "##" name)]
    [(unison-termlink-derived hash i) (describe-derived hash i)]))

(define (describe-bytes bs)
  (let* ([s (bytevector->base32-string bs #:alphabet 'hex)]
         [l (string-length s)]
         [sfx (if (<= l 10) "" "...")])
    (string-append "32x" (substring s 0 10) sfx)))

(define (describe-tuple x)
  (define (format-tuple l)
    (for/fold
      ([sep ")"]
       [bits '()]
       #:result (apply string-append (cons "(" bits)))
      ([e l])
      (values ", " (list* (describe-value e) sep bits))))

  (define (format-non-tuple l)
    (for/fold
      ([result #f])
      ([e l])
      (let ([de (describe-value e)])
        (if (not result) de
          (string-append "Cons (" de ") (" result ")")))))

  (let rec ([acc '()] [tup x])
    (match tup
      [(unison-data r t (list x y))
       #:when (eq? r ref-tuple:typelink)
       (rec (cons x acc) y)]
      [(unison-data r t (list))
       #:when (eq? r ref-unit:typelink)
       (format-tuple acc)]
      [else
       (format-non-tuple (cons tup acc))])))

(define (describe-applied f args)
  (string-append f " "))

(define (describe-value x)
  (match x
    [(unison-sum t fs)
     (let ([tt (number->string t)]
           [vs (describe-list-br fs)])
       (string-append "Sum " tt " " vs))]
    [(unison-data r t fs)
     #:when (eq? r ref-tuple:typelink)
     (describe-tuple x)]
    [(unison-data r t fs)
     (let ([tt (number->string t)]
           [rt (describe-ref r)]
           [vs (describe-list-br fs)])
       (string-append "{Data " rt " " tt " " vs "}"))]
    [(unison-pure v)
     (string-append "Pure " (describe-list-br (list v)))]
    [(? unison-termlink?) (termlink->string x #t)]
    [(? unison-typelink?) (typelink->string x #t)]
    [(unison-quote v)
     (string-append "{Value " (describe-value v) "}")]
    [(unison-code v)
     (string-append "{Code " (describe-value v) "}")]
    [(unison-cont-reflected fs) "{Continuation}"]
    [(unison-cont-wrapped _) "{Continuation}"]
    [(unison-closure gr code env)
     (define dc (groupref->string gr #t))
     (define (f v)
       (string-append " " (describe-value v)))

     (string-append* dc (map f env))]
    [(? procedure?) (describe-value (build-closure x))]
    [(? chunked-list?)
     (describe-list-sq (vector->list (chunked-list->vector x)))]
    [(? chunked-string?)
      (format "\"~a\"" (chunked-string->string x))]
    [(? chunked-bytes?)
     (format
      "0xs~a"
      (chunked-string->string
       (for/fold
         ([acc empty-chunked-string])
         ([n (in-chunked-bytes x)])
         (chunked-string-append acc (string->chunked-string (number->string n 16))))))]
    [(? list?) (describe-list-sq x)]
    ; [(ilist? x) (describe-list-sq (list->mlist x))]
    [(? number?) (number->string x)]
    [(? string?) (string-append "\"" x "\"")]
    [(? bytes?) (describe-bytes x)]
    [else
      (format "~a" x)]))

; Simple macro to expand a syntactic sequence of comparisons into a
; short-circuiting nested comparison.
(define-syntax comparisons
  (syntax-rules ()
    [(comparisons c) c]
    [(comparisons c d ...)
     (let ([sc c])
       (case sc
         ['= (comparisons d ...)]
         [else sc]))]))

; universal-compares two lists of values lexicographically
(define (lexico-compare ls rs cmp-ty)
  (let rec ([cls ls] [crs rs])
    (cond
      [(and (null? cls) (null? crs)) '=]
      [(null? cls) '<]
      [(null? crs) '>]
      [else
        (comparisons
          (universal-compare (car cls) (car crs) cmp-ty)
          (rec (cdr cls) (cdr crs)))])))

(define ((comparison e? l?) l r)
  (cond
    [(e? l r) '=]
    [(l? l r) '<]
    [else '>]))

(define compare-num (comparison = <))
(define compare-char (comparison char=? char<?))
(define compare-byte (comparison = <))
(define compare-bytes (comparison bytes=? bytes<?))
(define compare-string (comparison string=? string<?))

(define (compare-typelink ll rl)
  (match ll
    [(unison-typelink-builtin lnm)
     (match rl
       [(unison-typelink-builtin rnm) (compare-string lnm rnm)]
       [(? unison-typelink-derived?) '<])]
    [(unison-typelink-derived lh i)
     (match rl
       [(unison-typelink-derived rh j)
        (comparisons
          (compare-bytes lh rh)
          (compare-num i j))]
       [(? unison-typelink-builtin?) '>])]))

(define (compare-groupref lr rr)
  (match lr
    [(unison-groupref-builtin lname)
     (match rr
       [(unison-groupref-builtin rname)
        (compare-string lname rname)]
       [else '<])]
    [(unison-groupref-derived lh li ll)
     (match rr
       [(unison-groupref-derived rh ri rl)
        (comparisons
          (compare-bytes lh rh)
          (compare-num li ri)
          (compare-num ll rl))]
       [else '>])]))

(define (compare-termlink ll rl)
  (match ll
    [(unison-termlink-builtin lnm)
     (match rl
       [(unison-termlink-builtin rnm)
        (compare-string lnm rnm)]
       [else '<])]
    [(unison-termlink-derived lh i)
     (match rl
       [(unison-termlink-derived rh j)
        (comparisons
          (compare-bytes lh rh)
          (compare-num i j))]
       [(? unison-termlink-builtin?) '>]
       [else '<])]
    [(unison-termlink-con lr t)
     (match rl
       [(unison-termlink-con rr u)
        (comparisons
          (compare-typelink lr rr)
          (compare-num t u))]
       [else '>])]))

(define (value->category v)
  (cond
    [(unison-closure? v) 0]
    [(procedure? v) 0]
    [(number? v) 1]
    [(char? v) 1]
    [(boolean? v) 1]
    [(unison-data? v) 1]
    [(chunked-list? v) 3]
    [(chunked-string? v) 3]
    [(chunked-bytes? v) 3]
    [(unison-termlink? v) 3]
    [(unison-typelink? v) 3]
    [(bytes? v) 5]))

(define (compare-data l r cmp-ty)
  (match* (l r)
    [((unison-data lr lt lfs) (unison-data rr rt rfs))
     (compare-data-stuff lr lt lfs rr rt rfs cmp-ty)]))

(define (compare-data-stuff lr lt lfs rr rt rfs cmp-ty)
  (define new-cmp-ty (or cmp-ty (eq? lr builtin-any:typelink)))
  (comparisons
    (if cmp-ty (compare-typelink lr rr) '=)
    (compare-num lt rt)
    (compare-num (length lfs) (length rfs))
    (lexico-compare lfs rfs new-cmp-ty)))

; gives links to compare values as pseudo- or actual data types.
; This is how the interpreter works, so this is an attempt to obtain
; the same ordering.
(define (pseudo-data-link v)
  (cond
    [(boolean? v) builtin-boolean:typelink]
    [(char? v) builtin-char:typelink]
    [(flonum? v) builtin-float:typelink]
    [(and (number? v) (negative? v)) builtin-int:typelink]
    [(number? v) builtin-nat:typelink]
    [(unison-data? v) (unison-data-ref v)]))

(define (compare-proc l r cmp-ty)
  (define (unpack v)
    (define clo (build-closure v))

    (values
      (unison-closure-ref clo)
      (unison-closure-env clo)))

  (define-values (grl envl) (unpack l))

  (define-values (grr envr) (unpack r))

  (comparisons
    (compare-groupref grl grr)
    (lexico-compare envl envr cmp-ty)))

(define (compare-timespec l r)
  (comparisons
    (compare-num (unison-timespec-sec l) (unison-timespec-sec r))
    (compare-num (unison-timespec-nsec l) (unison-timespec-nsec r))))

(define (universal-compare l r [cmp-ty #f])
  (define (u-proc? v)
    (or (procedure? v) (unison-closure? v)))

  (cond
    [(eq? l r) '=] ; optimistic equality case
    [(and (boolean? l) (boolean? r)) (if r '< '>)]
    [(and (char? l) (char? r)) (if (char<? l r) '< '>)]
    [(and (number? l) (number? r)) (compare-num l r)]
    [(and (chunked-list? l) (chunked-list? r))
     (chunked-list-compare/recur l r universal-compare)]
    [(and (chunked-string? l) (chunked-string? r))
     (chunked-string-compare/recur l r compare-char)]
    [(and (chunked-bytes? l) (chunked-bytes? r))
     (chunked-bytes-compare/recur l r compare-byte)]
    [(and (unison-data? l) (unison-data? r)) (compare-data l r cmp-ty)]
    [(and (bytes? r) (bytes? r)) (compare-bytes l r)]
    [(and (u-proc? l) (u-proc? r)) (compare-proc l r cmp-ty)]
    [(and (unison-termlink? l) (unison-termlink? r))
     (compare-termlink l r)]
    [(and (unison-typelink? l) (unison-typelink? r))
     (compare-typelink l r)]
    [(and (unison-timespec? l) (unison-timespec? r))
     (compare-timespec l r)]
    [(= 3 (value->category l) (value->category r))
     (compare-typelink (pseudo-data-link l) (pseudo-data-link r))]
    [(= (value->category l) (value->category r))
     (raise
       (make-exn:bug
         "unsupported universal comparison of values"
         (unison-tuple l r)))]
    [else
      (compare-num (value->category l) (value->category r))]))


(define (list->unison-tuple l)
  (foldr ref-tuple-pair ref-unit-unit l))

(define (unison-tuple . l) (list->unison-tuple l))


(define (chunked-string<? l r) (chunked-string=?/recur l r char<?))

(define (universal=? l r)
  (define (pointwise ll lr)
    (let ([nl (null? ll)] [nr (null? lr)])
      (cond
        [(and nl nr) #t]
        [(or nl nr) #f]
        [else
          (and (universal=? (car ll) (car lr))
               (pointwise (cdr ll) (cdr lr)))])))
  (cond
    [(equal? l r) #t]
    [(and (unison-code? l) (unison-code? r))
     (universal=? (unison-code-rep l) (unison-code-rep r))]
    [(and (chunked-list? l) (chunked-list? r))
     (chunked-list=?/recur l r universal=?)]
    [(and (unison-data? l) (unison-data? r))
     (and
       (eqv? (unison-data-tag l) (unison-data-tag r))
       (pointwise (unison-data-fields l) (unison-data-fields r)))]
    [else #f]))

(define (exception->string e) (string->chunked-string (exn->string e)))

; (define (syntax->list stx)
;   (syntax-case stx ()
;     [() '()]
;     [(x . xs) (cons #'x (syntax->list #'xs))]))

(define-syntax let-marks
  (syntax-rules ()
    [(let-marks ks bn e ...)
     (call-with-marks ks bn (lambda () e ...))]))

(define (ref-mark k) (continuation-mark-set-first #f k))

(define (chunked-string-foldMap-chunks s m f)
  (for/fold
      ([acc empty-chunked-string])
      ([c (in-chunked-string-chunks s)])
    (f acc (string->chunked-string (m c)))))

(define (write-exn:bug ex port mode)
  (when mode (write-string "<exn:bug " port))

  (let ([recur (case mode
                 [(#t) write]
                 [(#f) display]
                 [else (lambda (v port) (print v port mode))])])
    (recur (exn:bug-msg ex) port)
    (if mode (write-string " " port) (newline port))
    (write-string (describe-value (exn:bug-val ex)) port))

  (when mode (write-string ">" port)))

(struct exn:bug (msg val)
  #:constructor-name make-exn:bug
  #:methods gen:custom-write
  [(define write-proc write-exn:bug)])


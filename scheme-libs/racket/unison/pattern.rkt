#lang racket/base

(require racket/contract
         racket/match
         "chunked-seq.rkt")

(provide (contract-out
          [pattern? predicate/c]
          [pattern-match (-> pattern?
                             chunked-string?
                             (or/c (cons/c chunked-string? chunked-list?) #f))]
          [pattern-match? (-> pattern? chunked-string? boolean?)]

          [eof pattern?]
          [any-char pattern?]
          [digit pattern?]
          [letter pattern?]
          [punctuation pattern?]
          [space pattern?]
          [alphanumeric pattern?]
          [upper pattern?]
          [lower pattern?]
          [number pattern?]
          [symbol pattern?]
          [control pattern?]
          [printable pattern?]
          [mark pattern?]
          [separator pattern?]

          [literal (-> chunked-string? pattern?)]
          [chars (-> chunked-list? pattern?)]
          [not-chars (-> chunked-list? pattern?)]
          [char-range (-> char? char? pattern?)]
          [not-char-range (-> char? char? pattern?)]
          [join (-> pattern? ... pattern?)]
          [join* (-> chunked-list? pattern?)]
          [choice (-> pattern? pattern? ... pattern?)]
          [capture (-> pattern? pattern?)]
          [many (-> pattern? pattern?)]
          [replicate (-> pattern? exact-nonnegative-integer? exact-nonnegative-integer? pattern?)]
          ;; Only valid pattern? in the functions below is p:char
          [char-class-and (-> pattern? pattern? pattern?)]
          [char-class-not (-> pattern? pattern?)]))

;; -----------------------------------------------------------------------------

(struct pattern (pat [maybe-matcher #:mutable]) #:transparent)

(define (make-pattern pat)
  (pattern pat #f))

(struct p:char
  (cpat) ; (or/c 'any (-> char? boolean?))
  #:transparent)
(struct p:literal (cstr) #:transparent)
(struct p:join (pats) #:transparent)
(struct p:or (left right) #:transparent)
(struct p:capture (pat) #:transparent)
(struct p:many (pat) #:transparent)
(struct p:replicate (pat min-count count))

;; -----------------------------------------------------------------------------

(define-syntax-rule (make-char-category-pred category-sym ...)
  (λ (c) (case (char-general-category c)
           [(category-sym ...) #t]
           [else #f])))

(define eof (make-pattern 'eof))
(define any-char (make-pattern (p:char 'any)))
(define digit (make-pattern (p:char (λ (c) (char<=? #\0 c #\9)))))
(define letter (make-pattern (p:char (make-char-category-pred lu ll lt lm lo))))
(define punctuation (make-pattern (p:char (make-char-category-pred pc pd ps pe pi pf po))))
(define space (make-pattern
               (p:char (λ (c) (case c
                                [(#\tab #\newline #\return #\page #\vtab) #t]
                                [else (eq? (char-general-category c) 'zs)])))))
(define alphanumeric (make-pattern (p:char (make-char-category-pred lu ll lt lm lo nd nl no))))
(define upper (make-pattern (p:char (make-char-category-pred lu lt))))
(define lower (make-pattern (p:char (make-char-category-pred ll))))
(define number (make-pattern (p:char (make-char-category-pred nd nl no))))
(define symbol  (make-pattern (p:char (make-char-category-pred sc sm sk so))))
(define control (make-pattern (p:char (make-char-category-pred cc))))
(define printable (make-pattern
                   (p:char (λ (c) (case (char-general-category c)
                                    [(zl zp cc cf cs co cn) #f]
                                    [else #t])))))
(define mark (make-pattern (p:char (make-char-category-pred mn mc me))))
(define separator (make-pattern (p:char (make-char-category-pred zs zl zp))))

(define (literal cstr)
  (make-pattern (p:literal cstr)))

(define (chars cstr)
  (make-pattern (p:char (λ (a) (for/or ([b (in-chunked-list cstr)]) (eqv? a b))))))
(define (not-chars cstr)
  (make-pattern (p:char (λ (a) (for/and ([b (in-chunked-list cstr)]) (not (eqv? a b)))))))

(define (char-range start end)
  (make-pattern (p:char (λ (c) (char<=? start c end)))))
(define (not-char-range start end)
  (make-pattern (p:char (λ (c) (not (char<=? start c end))))))

(define (join* pats)
  (make-pattern (p:join pats)))
;; Only used in tests
(define (join . pats)
  (join* (for/fold ([res empty-chunked-list])
                   ([e (in-list pats)])
           (chunked-list-add-last res e))))

(define choice
  (case-lambda
    [(pat) pat]
    [(left right)
     (make-pattern (p:or (pattern-pat left) (pattern-pat right)))]
    [pats
     (make-pattern
      (let loop ([pats pats])
        (match pats
          [(list pat)
           (pattern-pat pat)]
          [(cons pat pats)
           (p:or (pattern-pat pat) (loop pats))])))]))

(define (capture pat) (make-pattern (p:capture (pattern-pat pat))))
(define (many pat) (make-pattern (p:many (pattern-pat pat))))
(define (replicate pat n m) (make-pattern (p:replicate (pattern-pat pat) n m)))

;; -----------------------------------------------------------------------------

(define (pattern-match pat cstr)
  ((pattern-matcher pat) cstr))

;; Currently just does all the work of `pattern-match` and discards the
;; results. Could be made more efficient by avoiding doing capturing.
(define (pattern-match? pat cstr)
  (and (pattern-match pat cstr) #t))

(define (pattern-matcher pat)
  (cond
    [(pattern-maybe-matcher pat)]
    [else
     (define matcher (compile (pattern-pat pat)))
     (set-pattern-maybe-matcher! pat matcher)
     matcher]))

;; compile : (-> pat (-> chunked-string? (or/c (cons/c chunked-string? (chunked-list-of chunked-string?)) #f)))
(define (compile pat)
  (define (done cstr captures) (values cstr captures))
  (define (fail) (values #f #f))

  (define pat-m
    (let recur ([pat pat]
                [in-capture? #f]
                [ok done])
      (match pat
        ['eof
         (λ (cstr captures)
           (if (chunked-string-empty? cstr)
               (ok cstr captures)
               (fail)))]

        [(p:char 'any)
         (λ (cstr captures)
           (if (chunked-string-empty? cstr)
               (fail)
               (ok (chunked-string-drop-first cstr) captures)))]

        [(p:char predicate)
         (λ (cstr captures)
           (cond
             [(chunked-string-empty? cstr)
              (fail)]
             [else
              (define-values [cstr* c] (chunked-string-pop-first cstr))
              (if (predicate c)
                  (ok cstr* captures)
                  (fail))]))]

        [(p:literal lit-cstr)
         (define lit-len (chunked-string-length lit-cstr))
         (λ (cstr captures)
           (if (and (>= (chunked-string-length cstr) lit-len)
                    (for/and ([lit-c (in-chunked-string lit-cstr)]
                              [in-c (in-chunked-string cstr)])
                      (char=? lit-c in-c)))
               (ok (chunked-string-drop cstr lit-len) captures)
               (fail)))]

        [(p:join pats)
         (for/foldr ([ok ok])
                    ([pat (in-chunked-list pats)])
           (recur (pattern-pat pat) in-capture? ok))]

        [(p:or left right)
         (define left-m (recur left in-capture? done))
         (define right-m (recur right in-capture? ok))
         (λ (cstr captures)
           (define-values [cstr* captures*] (left-m cstr captures))
           (if cstr*
               (ok cstr* captures*)
               (right-m cstr captures)))]

        [(p:capture pat)
         (cond
           [in-capture?
            (recur pat #t ok)]
           [else
            (define pat-m (recur pat #t done))
            (λ (cstr captures)
              (define-values [cstr* captures*] (pat-m cstr captures))
              (cond
                [cstr*
                 (define capture-len (- (chunked-string-length cstr)
                                        (chunked-string-length cstr*)))
                 (define capture (chunked-string-take cstr capture-len))
                 (ok cstr* (chunked-list-add-last captures* capture))]
                [else
                 (fail)]))])]

        [(p:many (p:char 'any))
         (λ (cstr captures)
           (ok empty-chunked-string captures))]

        [(p:many (p:char predicate))
         (λ (cstr captures)
           (ok (chunked-string-drop-while cstr predicate) captures))]

        [(p:many pat)
         (define pat-m (recur pat in-capture? done))
         (λ (cstr captures)
           (let again ([cstr cstr]
                       [captures captures])
             (define-values [cstr* captures*] (pat-m cstr captures))
             (if cstr*
                 (again cstr* captures*)
                 (ok cstr captures))))]

        [(p:replicate pat min-count count)
         (define pat-m (recur pat in-capture? done))
         (define (replicate-rest cstr captures)
           (let again ([cstr cstr]
                       [captures captures]
                       [n (- count min-count)])
             (define-values [cstr* captures*] (pat-m cstr captures))
             (if (and cstr* (> n 0))
                 (again cstr* captures* (- n 1))
                 (ok cstr captures))))
         (λ (cstr captures)
           (define-values [cstr* captures*]
             (for/fold ([cstr cstr]
                        [captures captures]
                        #:result (ok cstr captures))
                       ([i (in-range min-count)])
               #:break (not cstr)
               (pat-m cstr captures)))
           (if (and cstr* (>= count min-count))
               (replicate-rest cstr* captures*)
               (ok cstr* captures*)))])))

  (λ (cstr)
    (define-values [cstr* captures] (pat-m cstr empty-chunked-list))
    (if cstr* (cons cstr* captures) #f)))


;; -----------------------------------------------------------------------------

(define (char-class-and cc1 cc2)
  (make-pattern
   (p:char
    (λ (c) (match (cons (pattern-pat cc1) (pattern-pat cc2))
             [(cons (p:char 'any) (p:char p)) (p c)]
             [(cons (p:char p) (p:char 'any)) (p c)]
             [(cons (p:char p1) (p:char p2)) (and (p1 c) (p2 c))])))))

(define (char-class-not cc)
  (make-pattern
   (p:char
    (λ (c) (match (pattern-pat cc)
             [(p:char 'any) #f]
             [(p:char p) (not (p c))])))))

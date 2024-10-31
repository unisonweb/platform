; This library implements various syntactic constructs and functions
; that are used in the compilation of unison (intermediate) source to
; scheme. The intent is to provide for writing scheme definitions that
; more directly match the source, so that the compiler doesn't need to
; emit all the code necessary to fix up the difference itself.
;
; Probably the best example of this is the define-unison macro, which
; looks similar to scheme's define, but the function being defined is
; allowed to be under/over applied similar to a unison function. It
; has an 'arity' at which computation happens, but the function
; automatically handles being applied to fewer or more arguments than
; that arity appropriately.
#!racket/base
(provide
  (all-from-out unison/data-info)
  builtin-any:typelink
  builtin-boolean:typelink
  builtin-bytes:typelink
  builtin-char:typelink
  builtin-float:typelink
  builtin-int:typelink
  builtin-nat:typelink
  builtin-text:typelink
  builtin-code:typelink
  builtin-mvar:typelink
  builtin-pattern:typelink
  builtin-promise:typelink
  builtin-sequence:typelink
  builtin-socket:typelink
  builtin-tls:typelink
  builtin-timespec:typelink
  builtin-threadid:typelink
  builtin-value:typelink

  builtin-crypto.hashalgorithm:typelink
  builtin-char.class:typelink
  builtin-immutablearray:typelink
  builtin-immutablebytearray:typelink
  builtin-mutablearray:typelink
  builtin-mutablebytearray:typelink
  builtin-processhandle:typelink
  builtin-ref.ticket:typelink
  builtin-tls.cipher:typelink
  builtin-tls.clientconfig:typelink
  builtin-tls.privatekey:typelink
  builtin-tls.serverconfig:typelink
  builtin-tls.signedcert:typelink
  builtin-tls.version:typelink

  builtin-udpsocket:typelink
  builtin-listensocket:typelink
  builtin-clientsockaddr:typelink

  bytevector
  bytes
  control
  define-unison
  define-unison-builtin
  handle
  name
  data
  data-case

  clamp-integer
  clamp-natural
  natural-max0
  wrap-natural
  bit64
  bit63
  nbit63

  expand-sandbox
  check-sandbox
  set-sandbox

  (struct-out unison-data)
  (struct-out unison-termlink)
  (struct-out unison-termlink-con)
  (struct-out unison-termlink-builtin)
  (struct-out unison-termlink-derived)
  (struct-out unison-typelink)
  (struct-out unison-typelink-builtin)
  (struct-out unison-typelink-derived)
  declare-function-link
  declare-code

  (struct-out exn:bug)
  exn:bug->exception
  exception->string
  raise-unison-exception

  exn:io?
  exn:arith?

  request
  request-case
  sum
  sum-case
  unison-force
  string->chunked-string
  empty-chunked-list

  identity

  describe-value
  decode-value
  describe-hash

  top-exn-handler

  reference->termlink
  reference->typelink
  referent->termlink
  typelink->reference
  termlink->referent
  termlink->reference

  unison-tuple->list
  list->unison-tuple
  unison-tuple
  unison-seq)

(require
  (for-syntax
    racket/set
    (only-in racket partition flatten split-at string-trim identity)
    (only-in racket/string string-prefix?)
    (only-in racket/syntax format-id))
  (rename-in
    (except-in racket false true unit any)
    [make-continuation-prompt-tag make-prompt])
  ; (for (only (compatibility mlist) mlist->list list->mlist) expand)
  ; (for (only (racket base) quasisyntax/loc) expand)
  ; (for-syntax (only-in unison/core syntax->list))
  (only-in racket/control control0-at)
  racket/performance-hint
  racket/trace
  unison/core
  unison/curry
  unison/data
  unison/sandbox
  unison/data-info
  (only-in unison/chunked-seq
           string->chunked-string
           chunked-string->string
           vector->chunked-list
           empty-chunked-list))

; Computes a symbol for automatically generated partial application
; cases, based on number of arguments applied. The partial
; application of `f` is (locally) named `f-partial-N`
; (meta define (partial-symbol name m)
;   (fun-sym (symbol->string name) "partial" (number->string m)))

; As above, but takes a syntactic object representing the arguments
; rather than their count.
; (define (partial-name name us)
;   (datum->syntax name (syntax->datum name)))

(define-syntax with-name
  (syntax-rules ()
    [(with-name name e) (let ([name e]) name)]))

; Our definition macro needs to generate multiple entry points for the
; defined procedures, so this is a function for making up names for
; those based on the original.
(define-for-syntax (adjust-symbol #:trim trim? name post)
  (define trimmer
    (if trim?
      (lambda (n) (string-trim n #px"-\\d+$"))
      identity))

  (string->symbol
    (string-append
      (trimmer (symbol->string name))
      ":"
      post)))

(define-for-syntax (adjust-name #:trim [trim? #f] name post)
  (datum->syntax name (adjust-symbol #:trim trim? (syntax->datum name) post) name))

(define-for-syntax (ref-link? name:link:stx)
  (string-prefix? (symbol->string (syntax->datum name:link:stx)) "ref-"))

(define-for-syntax (build-groupref internal? name:link:stx lo)
  (if (and internal? (ref-link? name:link:stx))
    #f
    #`(termlink->groupref #,name:link:stx #,lo)))

; Helper function. Turns a list of syntax objects into a
; list-syntax object.
(define-for-syntax (list->syntax l) #`(#,@l))

; These are auxiliary functions for manipulating a unison definition
; into a form amenable for the right runtime behavior. This involves
; multiple separate definitions:
;
; 1. an :impl definition is generated containing the actual code body
; 2. a :fast definition, which takes exactly the number of arguments
;    as the original, but checks if stack information needs to be
;    stored for continuation serialization.
; 3. a :slow path which implements under/over application to unison
;    definitions, so they act like curried functions, not scheme
;    procedures
; 4. a macro that implements the actual occurrences, and directly
;    calls the fast path for static calls with exactly the right
;    number of arguments
;
; Additionally, arguments are threaded through the internal
; definitions that indicate whether an ability handler is in place
; that could potentially result in the continuation being serialized.
; If so, then calls write additional information to the continuation
; for that serialization. This isn't cheap for tight loops, so we
; attempt to avoid this as much as possible (conditioning the
; annotation on a flag checkseems to cause no performance loss).


; This builds the core definition for a unison definition. It is just
; a lambda expression with the original code, but with an additional
; keyword argument for threading purity information.
(define-for-syntax (make-impl value? name:impl:stx arg:stx body:stx)
  (with-syntax ([name:impl name:impl:stx]
                [args arg:stx]
                [body body:stx])
    (cond
      [value?
        (syntax/loc body:stx
          (define name:impl . body))]
      [else
       (syntax/loc body:stx
         (define (name:impl . args) . body))])))

(define frame-contents (gensym))

; Builds the wrapper definition, 'fast path,' which just tests the
; purity, writes the stack information if necessary, and calls the
; implementation. If #:force-pure is specified, the fast path just
; directly calls the implementation procedure. This should allow
; tight loops to still perform well if we can detect that they
; (hereditarily) cannot make ability requests, even in contexts
; where a handler is present.
(define-for-syntax
  (make-fast-path
    #:force-pure force-pure?
    #:value value?
    loc ; original location
    name:fast:stx name:impl:stx
    arg:stx)

  (with-syntax ([name:impl name:impl:stx]
                [name:fast name:fast:stx]
                [args arg:stx])
    (cond
      [value?
       (syntax/loc loc
         (define (name:fast) name:impl))]

      [force-pure?
       (syntax/loc loc
         ; note: for some reason this performs better than
         ; (define name:fast name:impl)
         (define (name:fast . args) (name:impl . args)))]

      [else
       (syntax/loc loc
         (define (name:fast #:pure pure? . args)
           (if pure?
             (name:impl #:pure pure? . args)
             (with-continuation-mark
               frame-contents
               (vector . args)
               (name:impl #:pure pure? . args)))))])))

(define-for-syntax
  (make-main loc value? inline? name:stx ref:stx name:impl:stx n)
  (with-syntax ([name name:stx]
                [name:impl name:impl:stx]
                [gr ref:stx]
                [n (datum->syntax loc n)])
    (cond
      [value?
       (syntax/loc loc
         (define (name) name:impl))]
      [inline?
       (syntax/loc loc
         (define name
           (unison-curry #:inline n gr name:impl)))]
      [else
       (syntax/loc loc
         (define name
           (unison-curry n gr name:impl)))])))

(define-for-syntax
  (link-decl no-link-decl? loc name:stx name:fast:stx name:impl:stx)
  (if no-link-decl?
    #'()
    (let ([name:link:stx (adjust-name name:stx "termlink")])
      (with-syntax
        ([name:fast name:fast:stx]
         [name:impl name:impl:stx]
         [name:link name:link:stx])
        (syntax/loc loc
          ((declare-function-link name:fast name:link)
           (declare-function-link name:impl name:link)))))))

(define-for-syntax
  (trace-decls trace? loc name:impl:stx)
  (if trace?
    (with-syntax ([name:impl name:impl:stx])
      (syntax/loc loc
        ((trace name:impl))))
    #'()))

(define-for-syntax (process-hints hs)
  (for/fold ([internal? #f]
             [force-pure? #t]
             [gen-link? #f]
             [no-link-decl? #f]
             [trace? #f]
             [inline? #f]
             [recursive? #f]
             [value? #f])
            ([h hs])
    (values
      (or internal? (eq? h 'internal))
      (or force-pure? (eq? h 'force-pure) (eq? h 'internal))
      (or gen-link? (eq? h 'gen-link))
      (or no-link-decl? (eq? h 'no-link-decl))
      (or trace? (eq? h 'trace))
      (or inline? (eq? h 'inline))
      (or recursive? (eq? h 'recursive))
      ; TODO: enable values
      value?)))

(define-for-syntax
  (make-link-def gen-link? loc name:stx name:link:stx)

  (define (chop s)
    (if (string-prefix? s "builtin-")
      (substring s 8)
      s))

  (define name:txt
    (chop
      (symbol->string
        (syntax->datum name:stx))))

  (cond
    [gen-link?
      (with-syntax ([name:link name:link:stx])
        (quasisyntax/loc loc
          ((define name:link
             (unison-termlink-builtin #,name:txt)))))]
    [else #'()]))

(define-for-syntax
  (expand-define-unison
    #:hints hints
    #:local [lo 0]
    loc name:stx arg:stx expr:stx)

  (define-values (internal?
                  force-pure?
                  gen-link?
                  no-link-decl?
                  trace?
                  inline?
                  recursive?
                  value?)
    (process-hints hints))


  (let* ([name:fast:stx (adjust-name name:stx "fast")]
         [name:impl:stx (adjust-name name:stx "impl")]
         [name:link:stx (adjust-name name:stx "termlink" #:trim #t)]
         [ref:stx (build-groupref internal? name:link:stx lo)]
         [arity (length (syntax->list arg:stx))])
    (with-syntax
      ([(link ...) (make-link-def gen-link? loc name:stx name:link:stx)]
       [fast (make-fast-path
               #:force-pure #t ; force-pure?
               #:value value?
               loc name:fast:stx name:impl:stx arg:stx)]
       [impl (make-impl value? name:impl:stx arg:stx expr:stx)]
       [main (make-main loc value? inline? name:stx ref:stx name:impl:stx arity)]
       ; [(decls ...)
       ;  (link-decl no-link-decl? loc name:stx name:fast:stx name:impl:stx)]
       [(traces ...)
        (trace-decls trace? loc name:impl:stx)])
      (quasisyntax/loc loc
        (begin
          link ...
          #,(if (or recursive? inline?) #'(begin-encourage-inline impl) #'impl)
          traces ...
          #,(if (or recursive? inline?) #'(begin-encourage-inline fast) #'fast)
          #,(if inline? #'(begin-encourage-inline main) #'main))))))

; Function definition supporting various unison features, like
; partial application and continuation serialization. See above for
; details.
;
; `#:internal #t` indicates that the definition is for builtin
; functions. These should always be built in a way that does not
; annotate the stack, because they don't make relevant ability
; requests. This is important for performance and some correct
; behavior (i.e. they may occur in non-unison contexts where a
; `pure?` indicator is not being threaded).
(define-syntax (define-unison stx)
  (syntax-case stx ()
    [(define-unsion #:hints hs #:local n (name . args) . exprs)
     (expand-define-unison
       #:local (syntax->datum #'n)
       #:hints (syntax->datum #'hs)
       stx #'name #'args #'exprs)]
    [(define-unsion #:local n #:hints hs (name . args) . exprs)
     (expand-define-unison
       #:local (syntax->datum #'n)
       #:hints (syntax->datum #'hs)
       stx #'name #'args #'exprs)]
    [(define-unison #:hints hs (name . args) . exprs)
     (expand-define-unison
       #:hints (syntax->datum #'hs)
       stx #'name #'args #'exprs)]
    [(define-unison #:local n (name . args) . exprs)
     (expand-define-unison
       #:local (syntax->datum #'n)
       #:hints '[]
       stx #'name #'args #'exprs)]
    [(define-unison (name . args) . exprs)
     (expand-define-unison
       #:hints '[internal]
       stx #'name #'args #'exprs)]))

(define-syntax (define-unison-builtin stx)
  (syntax-case stx ()
    [(define-unison-builtin #:local n #:hints [h ...] . rest)
     (syntax/loc stx
       (define-unison #:local n #:hints [inline internal gen-link h ...] . rest))]
    [(define-unison-builtin #:local n . rest)
     (syntax/loc stx
       (define-unison #:local n #:hints [inline internal gen-link] . rest))]
    [(define-unison-builtin #:hints [h ...] . rest)
     (syntax/loc stx
       (define-unison #:hints [inline internal gen-link h ...] . rest))]
    [(define-unison-builtin . rest)
     (syntax/loc stx
       (define-unison #:hints [inline internal gen-link] . rest))]))

; call-by-name bindings
(define-syntax (name stx)
  (syntax-case stx ()
    [(name ([v (f . args)] ...) body ...)
     (syntax/loc stx
       (let ([v (build-closure f . args)] ...) body ...))]))

; Wrapper that more closely matches `handle` constructs
(define-syntax handle
  (syntax-rules ()
    [(handle [r ...] h e ...)
     (call-with-handler '(r ...) h (lambda () e ...))]))

; wrapper that more closely matches ability requests
(define-syntax request
  (syntax-rules ()
    [(request r t . args)
     (let* ([key (quote r)]
            [rq (make-request key t (list . args))]
            [current-mark (ref-mark key)])
       (if (pair? current-mark)
         ((cdr current-mark) rq)
         (error "unhandled ability request: " (list key t . args))))]))

; See the explanation of `handle` for a more thorough understanding
; of why this is doing two control operations.
;
; In-unison 'control' corresponds to a (shallow) handler jump, so we
; need to capture the continuation _and_ discard some dynamic scope
; information. The capture is accomplished via the first
; control0-at, while the second does the discard, based on the
; convention used in `handle`.
(define-syntax control
  (syntax-rules ()
    [(control r k e ...)
     (let ([p (car (ref-mark (quote r)))])
       (control0-at p k (control0-at p _k e ...)))]))

; forces something that is expected to be a thunk, defined with
; e.g. `name` above. In some cases, we might have a normal value,
; so just do nothing in that case.
(define (unison-force x)
  (if (procedure? x) (x) x))

; If #t, causes sum-case and data-case to insert else cases if
; they don't have one. The inserted case will report the covered
; cases and which tag was being matched.
(define-for-syntax debug-cases #t)

(define-for-syntax (tag? s)
  (and (syntax? s) (fixnum? (syntax->datum s))))

(define-for-syntax (tags? s)
  (andmap tag? (syntax->list s)))

(define-for-syntax (identifiers? s)
  (andmap identifier? (syntax->list s)))

(define-for-syntax (process-cases mac-name stx scstx tgstx flstx cs)
  (define (raiser msg sub)
    (raise-syntax-error #f msg stx sub))

  (define (raise-else sub)
    (raiser
      (string-append "else clause must be final in " mac-name)
      sub))

  (define (raise-tags sub)
    (raiser
      (string-append "non-tags used in " mac-name " branch")
      sub))

  (define (raise-vars sub)
    (raiser
      (string-append "non-variables used in " mac-name " binding")
      sub))

  (define (has-else? c)
    (syntax-case c (else)
      [(else . x) #t]
      [_ #f]))

  (define (syntax->tags ts)
    (list->set (map syntax->datum (syntax->list ts))))

  (define (process-case head tail)
    (with-syntax ([fields flstx] [scrut scstx])
      (syntax-case head (else)
        [(else e ...)
         (syntax-case tail ()
           [() (values (set) head)] ; case is already in the right form
           [_ (raise-else head)])]
        [((t ...) () e ...)
         (cond
           [(not (tags? #'(t ...))) (raise-tags head)]
           [else
             (values
               (syntax->tags #'(t ...))
               #'((t ...) e ...))])]
        [(t () e ...)
         (cond
           [(not (tag? #'t)) (raise-tags head)]
           [else
             (values
               (set (syntax->datum #'t))
               #'((t) e ...))])]
        [((t ...) (v ...) e ...)
         (cond
           [(not (tags? #'(t ...))) (raise-tags head)]
           [(not (identifiers? #'(v ...))) (raise-vars head)]
           [else
             (values
               (syntax->tags #'(t ...))
               #'((t ...)
                  (let-values
                    ([(v ...) (apply values (fields scrut))])
                    e ...)))])]
        [(t (v ...) e ...)
         (cond
           [(not (tag? #'t)) (raise-tags head)]
           [(not (identifiers? #'(v ...))) (raise-vars head)]
           [else
             (values
               (set (syntax->datum #'t))
               #'((t)
                  (let-values
                    ([(v ...) (apply values (fields scrut))])
                    e ...)))])]
        [((t ...) v e ...)
         (cond
           [(not (tags? #'(t ...))) (raise-tags head)]
           [(not (identifier? #'v)) (raise-vars head)]
           [else
             (values
               (syntax->tags #'(t ...))
               #'((t ...) (let ([v (fields scrut)]) e ...)))])]
        [(t v e ...)
         (cond
           [(not (tag? #'t)) (raise-tags head)]
           [(not (identifier? #'v)) (raise-vars head)]
           [else
             (values
               (set (syntax->datum #'t))
               #'((t) (let ([v (fields scrut)]) e ...)))])])))

  (define (build-else sts)
    (with-syntax ([tag tgstx])
      #`(else
          (let* ([ts (list #,@sts)]
                 [tg (tag #,scstx)]
                 [fmst "~a: non-exhaustive match:\n~a\n~a"]
                 [cst (format "      tag: ~v" tg)]
                 [tst (format "  covered: ~v" ts)]
                 [msg (format fmst #,mac-name cst tst)])
            (raise msg)))))

  (let rec ([el (not debug-cases)]
            [tags (list->set '())]
            [acc '()]
            [cur cs])
    (syntax-case cur ()
      [()
       (let ([acc (if el acc (cons (build-else (set->list tags)) acc))])
         (reverse acc))]
      [(head . tail)
       (let-values ([(ts pc) (process-case #'head #'tail)])
         (rec
           (or el (has-else? #'head))
           (set-union tags ts)
           (cons pc acc)
           #'tail))])))

(define-syntax sum-case
  (lambda (stx)
    (syntax-case stx ()
      [(sum-case scrut c ...)
       (with-syntax ([(tc ...)
                      (process-cases
                        "sum-case"
                        stx
                        #'scrut
                        #'unison-sum-tag
                        #'unison-sum-fields
                        #'(c ...))])
         #'(case (unison-sum-tag scrut) tc ...))])))

(define-syntax data-case
  (lambda (stx)
    (syntax-case stx ()
      [(data-case scrut c ...)
       (with-syntax ([(tc ...)
                      (process-cases
                        "data-case"
                        stx
                        #'scrut
                        #'unison-data-tag
                        #'unison-data-fields
                        #'(c ...))])
         (syntax/loc stx
           (case (unison-data-tag scrut) tc ...)))])))

(define-syntax request-case
  (lambda (stx)
    (define (pure-case? c)
      (syntax-case c (pure)
        [(pure . xs) #t]
        [_ #f]))

    (define (mk-pure ps)
      (if (null? ps)
        #'((unison-pure v) v)
        (syntax-case (car ps) (pure)
          [(pure (v) e ...) #'((unison-pure v) e ...)]
          [(pure vs e ...)
           (raise-syntax-error
             #f
             "pure cases receive exactly one variable"
             (car ps)
             #'vs)])))

    (define (mk-req stx)
      (syntax-case stx ()
        [(t (v ...) e ...)
         #'((t (list v ...)) e ...)]))

    (define (mk-abil scrut-stx)
      (lambda (stx)
        (syntax-case stx ()
          [(a sc ...)
           #`((unison-request b t vs)
              #:when (eq? (quote a) b)
              (match* (t vs)
                #,@(map mk-req (syntax->list #'(sc ...)))))])))

    (syntax-case stx ()
      [(request-case scrut c ...)
       (let-values
         ([(ps as) (partition pure-case? (syntax->list #'(c ...)))])
         (if (> 1 (length ps))
           (raise-syntax-error
             #f
             "multiple pure cases in request-case"
             stx)
           (with-syntax
             ([pc (mk-pure ps)]
              [(ac ...) (map (mk-abil #'scrut) as)])

             #'(match scrut pc ac ...))))])))

(define (decode-value x) '())

(define (reference->termlink rf)
  (match rf
    [(unison-data _ t (list nm))
     #:when (= t ref-reference-builtin:tag)
     (unison-termlink-builtin (chunked-string->string nm))]
    [(unison-data _ t (list id))
     #:when (= t ref-reference-derived:tag)
     (match id
       [(unison-data _ t (list rf i))
        #:when (= t ref-id-id:tag)
        (unison-termlink-derived rf i)])]
    [else
     (raise-argument-error
       'reference->termlink
       "unison-reference?"
       rf)]))

(define (referent->termlink rn)
  (match rn
    [(unison-data _ t (list rf i))
     #:when (= t ref-referent-con:tag)
     (unison-termlink-con (reference->typelink rf) i)]
    [(unison-data _ t (list rf))
     #:when (= t ref-referent-def:tag)
     (reference->termlink rf)]))

(define (reference->typelink rf)
  (match rf
    [(unison-data _ t (list nm))
     #:when (= t ref-reference-builtin:tag)
     (unison-typelink-builtin (chunked-string->string nm))]
    [(unison-data _ t (list id))
     #:when (= t ref-reference-derived:tag)
     (match id
       [(unison-data _ t (list rf i))
        #:when (= t ref-id-id:tag)
        (unison-typelink-derived rf i)])]))

(define (typelink->reference tl)
  (match tl
    [(unison-typelink-builtin nm)
     (ref-reference-builtin (string->chunked-string nm))]
    [(unison-typelink-derived hs i)
     (ref-reference-derived (ref-id-id hs i))]))

(define (termlink->referent tl)
  (match tl
    [(unison-termlink-builtin nm)
     (ref-referent-def
       (ref-reference-builtin nm))]
    [(unison-termlink-derived rf i)
     (ref-referent-def
       (ref-reference-derived
         (ref-id-id rf i)))]
    [(unison-termlink-con tyl i)
     (ref-referent-con (typelink->reference tyl) i)]))

(define (termlink->reference rn)
  (match rn
    [(unison-termlink-builtin name)
     (ref-reference-builtin
       (string->chunked-string name))]
    [(unison-termlink-derived bs i)
     (ref-reference-derived (ref-id-id bs i))]
    [else (raise "termlink->reference: con case")]))


(define (unison-seq . l)
  (vector->chunked-list (list->vector l)))

; Top level exception handler, moved from being generated in unison.
; The in-unison definition was effectively just literal scheme code
; represented as a unison data type, with some names generated from
; codebase data.
(define (top-exn-handler rq)
  (request-case rq
    [pure (x)
      (match x
        [(unison-data r 0 (list))
         (eq? r ref-unit:typelink)
         (display "")]
        [else
          (display (describe-value x))])]
    [ref-exception
      [0 (f)
       (control ref-exception k
         (let ([disp (describe-value f)])
           (raise
             (make-exn:bug
               (string->chunked-string "unhandled top level exception")
               disp))))]]))

(begin-encourage-inline
  (define mask64 #xffffffffffffffff)
  (define mask63 #x7fffffffffffffff)
  (define bit63 #x8000000000000000)
  (define bit64 #x10000000000000000)
  (define nbit63 (- #x8000000000000000))

  ; Operation to maintain Int values to within a range from
  ; -2^63 to 2^63-1.
  (define (clamp-integer i)
    (if (fixnum? i) i
      (let ([j (bitwise-and mask64 i)])
        (if (< j bit63) j
          (- j bit64)))))

  ; modular arithmetic appropriate for when a Nat operation can only
  ; overflow (be too large a positive number).
  (define (clamp-natural n)
    (if (fixnum? n) n
      (modulo n bit64)))

  ; For natural arithmetic operations that can yield negatives, this
  ; ensures that they are clamped back to 0.
  ;
  ; Note: (max 0 n) is apparently around 2-3x slower than this, hence
  ; the custom operation. I've factored it out here in case something
  ; even better is found, but this seems to match the performance of
  ; the underlying operation.
  (define (natural-max0 n) (if (>= n 0) n 0))

  ; module arithmetic appropriate for when a Nat operation my either
  ; have too large or a negative result.
  (define (wrap-natural n)
    (if (and (fixnum? n) (exact-nonnegative-integer? n)) n
      (modulo n bit64))))

(define (raise-unison-exception ty msg val)
  (request
    ref-exception
    0
    (ref-failure-failure ty msg (unison-any-any val))))

(define (exn:bug->exception b)
  (raise-unison-exception
    ref-runtimefailure:typelink
    (string->chunked-string (exn:bug-msg b))
    (exn:bug-val b)))

(define (exn:io? e)
  (or (exn:fail:read? e)
      (exn:fail:filesystem? e)
      (exn:fail:network? e)))

(define (exn:arith? e)
  (or (exn:fail:contract:divide-by-zero? e)
      (exn:fail:contract:non-fixnum-result? e)))


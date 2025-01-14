;; Helpers for building data that conform to the compiler calling convention

#!racket/base
(provide
  declare-unison-data-hash
  data-hash->number
  data-number->hash
  declare-function-link
  lookup-function-link
  declare-code
  lookup-code
  have-code?

  (struct-out unison-data)
  (struct-out unison-continuation)
  (struct-out unison-cont-wrapped)
  (struct-out unison-cont-reflected)
  (struct-out unison-frame)
  (struct-out unison-frame-push)
  (struct-out unison-frame-mark)
  (struct-out unison-sum)
  (struct-out unison-pure)
  (struct-out unison-request)
  (struct-out unison-closure)
  (struct-out unison-termlink)
  (struct-out unison-termlink-con)
  (struct-out unison-termlink-builtin)
  (struct-out unison-termlink-derived)
  (struct-out unison-typelink)
  (struct-out unison-typelink-builtin)
  (struct-out unison-typelink-derived)
  (struct-out unison-groupref)
  (struct-out unison-groupref-builtin)
  (struct-out unison-groupref-derived)
  (struct-out unison-code)
  (struct-out unison-quote)
  (struct-out unison-timespec)

  build-closure

  call-with-handler
  call-with-marks

  define-builtin-link
  declare-builtin-link

  data
  sum
  partial-app

  some
  none
  some?
  none?
  option-get
  right
  left
  right?
  left?
  either-get
  either-get
  sum-unit
  sum-false
  sum-true
  bool
  char
  ord
  failure
  exception

  builtin-any:typelink
  unison-any-any:tag
  unison-any-any

  builtin-boolean:typelink
  unison-boolean-true:tag
  unison-boolean-false:tag
  unison-boolean-true
  unison-boolean-false

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
  builtin-udpsocket:typelink
  builtin-listensocket:typelink
  builtin-clientsockaddr:typelink

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

  unison-tuple->list
  unison-pair->cons

  typelink->string
  termlink->string
  groupref->string

  groupref->termlink
  termlink->groupref)

(require
  (rename-in racket
    [make-continuation-prompt-tag make-prompt])
  (only-in racket/control prompt0-at control0-at)
  racket/fixnum
  (only-in "vector-trie.rkt" ->fx/wraparound)
  unison/bytevector)

(struct unison-data
  (ref tag fields)
  #:sealed
  #:transparent
  #:constructor-name make-data
  #:property prop:equal+hash
  (let ()
    (define (equal-proc data-l data-r rec)
      (and
        (= (unison-data-tag data-l) (unison-data-tag data-r))
        (andmap rec
                (unison-data-fields data-l)
                (unison-data-fields data-r))))

    (define ((hash-proc init) d rec)
      (for/fold ([hc init])
                ([v (unison-data-fields d)])
        (fxxor (fx*/wraparound hc 31)
               (->fx/wraparound (rec v)))))

    (list equal-proc (hash-proc 3) (hash-proc 5))))

(define (data r t . args) (make-data r t args))

(struct unison-sum
  (tag fields)
  #:constructor-name make-sum)

(define (sum t . args) (make-sum t args))

(struct unison-pure
  (val)
  #:constructor-name make-pure)

(struct unison-request
  (ability tag fields)
  #:constructor-name make-request
  #:transparent)

; Structures for other unison builtins. Originally the plan was
; just to secretly use an in-unison data type representation.
; However, there are generic functions written in scheme that
; do not have access to typing informatiaon, and it becomes
; impossible to distinguish data type values from pseudo-builtins
; using the same representation, while the behavior must be
; different. So, at least, we must wrap the unison data in
; something that allows us to distinguish it as builtin.
(struct unison-termlink ()
  #:transparent
  #:reflection-name 'termlink
  #:methods gen:custom-write
  [(define (write-proc tl port mode)
     (write-string (termlink->string tl #t) port))]
  #:property prop:equal+hash
  (let ()
    (define (equal-proc lnl lnr rec)
      (match lnl
        [(unison-termlink-con r i)
         (match lnr
           [(unison-termlink-con l j)
            (and (rec r l) (= i j))]
           [else #f])]
        [(unison-termlink-builtin l)
         (match lnr
           [(unison-termlink-builtin r)
            (equal? l r)]
           [else #f])]
        [(unison-termlink-derived hl i)
         (match lnr
           [(unison-termlink-derived hr j)
            (and (equal? hl hr) (= i j))]
           [else #f])]))

    (define ((hash-proc init) ln rec)
      (match ln
        [(unison-termlink-con r i)
         (fxxor (fx*/wraparound (rec r) 29)
                (fx*/wraparound (rec i) 23)
                (fx*/wraparound init 17))]
        [(unison-termlink-builtin n)
         (fxxor (fx*/wraparound (rec n) 31)
                (fx*/wraparound init 13))]
        [(unison-termlink-derived hl i)
         (fxxor (fx*/wraparound (rec hl) 37)
                (fx*/wraparound (rec i) 41)
                (fx*/wraparound init 7))]))

    (list equal-proc (hash-proc 3) (hash-proc 5))))

(struct unison-termlink-con unison-termlink
  (ref index)
  #:reflection-name 'termlink)

(struct unison-termlink-builtin unison-termlink
  (name)
  #:reflection-name 'termlink)

(struct unison-termlink-derived unison-termlink
  (hash i)
  #:reflection-name 'termlink)

; A groupref is like a termlink, but is used for reflection of
; functions. As such, there is no con case. Also, there's an extra
; level of indexing involved in grouprefs, because multiple scheme
; functions can be generated from the same top level unison
; definition, even after floating.
(struct unison-groupref ()
  #:methods gen:custom-write
  [(define (write-proc gr port mode)
     (write-string (groupref->string gr #t) port))]
  #:property prop:equal+hash
  (let ()
    (define (equal-proc grl grr rec)
      (match grl
        [(unison-groupref-builtin nl)
         (match grr
           [(unison-groupref-builtin nr)
            (rec nl nr)]
           [else #f])]
        [(unison-groupref-derived hl il ll)
         (match grr
           [(unison-groupref-derived hr ir lr)
            (and (rec hl hr) (= il ir) (= ll lr))]
           [else #f])]))

    (define ((hash-proc init) gr rec)
      (match gr
        [(unison-groupref-builtin n)
         (fxxor (fx*/wraparound (rec n) 113)
                (fx*/wraparound init 109))]
        [(unison-groupref-derived h i l)
         (fxxor (fx*/wraparound (rec h) 127)
                (fx*/wraparound (rec i) 131)
                (fx*/wraparound (rec l) 137))]))

    (list equal-proc (hash-proc 3) (hash-proc 5))))

(struct unison-groupref-builtin unison-groupref
  (name))

(struct unison-groupref-derived unison-groupref
  (hash index local))

(struct unison-typelink ()
  #:transparent
  #:reflection-name 'typelink
  #:methods gen:custom-write
  [(define (write-proc tl port mode)
     (write-string (typelink->string tl #t) port))]
  #:property prop:equal+hash
  (let ()
    (define (equal-proc lnl lnr rec)
      (match lnl
        [(unison-typelink-builtin l)
         (match lnr
           [(unison-typelink-builtin r)
            (equal? l r)]
           [else #f])]
        [(unison-typelink-derived hl i)
         (match lnr
           [(unison-typelink-derived hr j)
            (and (equal? hl hr) (= i j))]
           [else #f])]))

    (define ((hash-proc init) ln rec)
      (match ln
        [(unison-typelink-builtin n)
         (fxxor (fx*/wraparound (rec n) 53)
                (fx*/wraparound init 17))]
        [(unison-typelink-derived hl i)
         (fxxor (fx*/wraparound (rec hl) 59)
                (fx*/wraparound (rec i) 61)
                (fx*/wraparound init 19))]))

    (list equal-proc (hash-proc 3) (hash-proc 5))))

(struct unison-typelink-builtin unison-typelink
  (name)
  #:reflection-name 'typelink
  #:transparent)

(struct unison-typelink-derived unison-typelink
  (ref ix)
  #:reflection-name 'typelink
  #:transparent)

(struct unison-code (rep))
(struct unison-quote (val))

(define (write-procedure f port mode)
  (cond
    [(hash-has-key? function-associations f)
     (define tl (lookup-function-link f))
     (write-string (termlink->string tl #t) port)]
    [else
     (case mode
       [(#f) (display f port)]
       [(#t) (write f port)]
       [else (print f port mode)])]))

(define (write-sequence s port mode)
  (define rec
    (case mode
      [(#f) display]
      [(#t) write]
      [else (lambda (e port) (print e port mode))]))

  (write-string "'(" port)

  (define first #t)

  (for ([e s])
    (unless first
      (write-string " " port)
      (set! first #f))

    (if (procedure? e)
      (write-procedure e port mode)
      (rec e port)))
  (write-string ")" port))

(struct unison-closure
  (ref code env)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc clo port mode)
     (define rec
       (case mode
         [(#t) write]
         [(#f) display]
         [else (lambda (v port) (print v port mode))]))

     (write-string "(unison-closure " port)
     (write-procedure (unison-closure-code clo) port mode)
     (write-string " " port)
     (write-sequence (unison-closure-env clo) port mode)
     (write-string ")" port))]

  ; This has essentially becomes the slow path for unison function
  ; application. The definition macro immediately creates a closure
  ; for any statically under-saturated call or unapplied occurrence.
  ; This means that there is never a bare unison function being passed
  ; as a value. So, we can define the slow path here once and for all.
  #:property prop:procedure
  (lambda (clo . rest)
    (define code (unison-closure-code clo))
    (define arity (procedure-arity code))
    (define old-env (unison-closure-env clo))

    (define new-env (append old-env rest))
    (define k (length rest))
    (define l (length new-env))
    (cond
      [(= arity l) ; saturated
       (apply code new-env)]
      [(= k 0) clo] ; special case, 0-applying undersaturated
      [(< arity l)
       ; TODO: pending arg annotation if no pure?
       (define-values (now pending) (split-at new-env arity))
       (apply (apply code now) pending)]
      [else ; still undersaturated
        (struct-copy unison-closure clo [env new-env])])))

(define (reflect-procedure f)
  (if (unison-closure? f)
    f
    (let-values ([(req opt) (procedure-keywords f)])
      (if (member '#:reflect opt)
        ; 0-arg case
        (f #:reflect #t)
        ; otherwise, by convention, applying enough to 0 args reflects
        ((f))))))

(define (build-closure f . args)
  (define clo (reflect-procedure f))
  (define env (unison-closure-env clo))

  (struct-copy unison-closure clo [env (append env args)]))

(struct unison-timespec (sec nsec)
  #:transparent
  #:property prop:equal+hash
  (let ()
    (define (equal-proc tml tmr rec)
      (match tml
        [(unison-timespec sl nsl)
         (match tmr
           [(unison-timespec sr nsr)
            (and (= sl sr) (= nsl nsr))])]))

    (define ((hash-proc init) tm rec)
      (match tm
        [(unison-timespec s ns)
         (fxxor (fx*/wraparound (rec s) 67)
                (fx*/wraparound (rec ns) 71)
                (fx*/wraparound init 73))]))

    (list equal-proc (hash-proc 3) (hash-proc 5))))

; This is the base struct for continuation representations. It has
; two possibilities seen below.
(struct unison-continuation () #:transparent)

; This is a wrapper that allows for a struct representation of all
; continuations involved in unison. I.E. instead of just passing
; around a raw racket continuation, we wrap it in a box for easier
; identification.
(struct unison-cont-wrapped unison-continuation (cont)
  ; Use the wrapped continuation for procedure calls. Continuations
  ; will always be called via the jumpCont wrapper which exactly
  ; applies them to one argument.
  #:property prop:procedure 0)

; Basic mechanism for installing handlers, defined here so that it
; can be used in the implementation of reflected continuations.
;
; Note: this uses the prompt _twice_ to achieve the sort of dynamic
; scoping we want. First we push an outer delimiter, then install
; the continuation marks corresponding to the handled abilities
; (which tells which propt to use for that ability and which
; functions to use for each request). Then we re-delimit by the same
; prompt.
;
; If we just used one delimiter, we'd have a problem. If we pushed
; the marks _after_ the delimiter, then the continuation captured
; when handling would contain those marks, and would effectively
; retain the handler for requests within the continuation. If the
; marks were outside the prompt, we'd be in a similar situation,
; except where the handler would be automatically handling requests
; within its own implementation (although, in both these cases we'd
; get control errors, because we would be using the _function_ part
; of the handler without the necessary delimiters existing on the
; continuation). Both of these situations are wrong for _shallow_
; handlers.
;
; Instead, what we need to be able to do is capture the continuation
; _up to_ the marks, then _discard_ the marks, and this is what the
; multiple delimiters accomplish. There might be more efficient ways
; to accomplish this with some specialized mark functions, but I'm
; uncertain of what pitfalls there are with regard to that (whehter
; they work might depend on exact frame structure of the
; metacontinuation).
(define (call-with-handler rs h f)
  (let ([p (make-prompt)])
    (prompt0-at p
      (let ([v (call-with-marks rs (cons p h)
                 (lambda () (prompt0-at p (f))))])
        (h (make-pure v))))))

(define (call-with-marks rs v f)
  (cond
    [(null? rs) (f)]
    [else
      (with-continuation-mark (car rs) v
        (call-with-marks (cdr rs) v f))]))

; Version of the above for re-installing a handlers in the serialized
; format. In that case, there is an association list of links and
; handlers, rather than a single handler (although the separate
; handlers are likely duplicates).
(define (call-with-assoc-marks p hs f)
  (match hs
    ['() (f)]
    [(cons (cons r h) rest)
     (with-continuation-mark r (cons p h)
       (call-with-assoc-marks rest f))]))

(define (call-with-handler-assocs hs f)
  (let ([p (make-prompt)])
    (prompt0-at p
      (call-with-assoc-marks p hs
        (lambda () (prompt0-at p (f)))))))

(define (repush frames v)
  (match frames
    ['() v]
    [(cons (unison-frame-mark as tls hs) frames)
     ; handler frame; as are pending arguments, tls are typelinks
     ; for handled abilities; hs are associations from links to
     ; handler values.
     ;
     ; todo: args
     (call-with-handler-assocs hs
       (lambda () (repush frames v)))]
    [(cons (unison-frame-push ls as rt) rest)
     (displayln (list ls as rt))
     (raise "repush push: not implemented yet")]))

; This is a *reflected* representation of continuations amenable
; to serialization. Most continuations won't be in this format,
; because it's foolish to eagerly parse the racket continuation if
; it's just going to be applied. But, a continuation that we've
; gotten from serialization will be in this format.
;
; `frames` should be a list of the below `unison-frame` structs.
(struct unison-cont-reflected unison-continuation (frames)
  #:property prop:procedure
  (lambda (cont v) (repush (unison-cont-reflected-frames cont) v)))

; Stack frames for reflected continuations
(struct unison-frame () #:transparent)

(struct unison-frame-push unison-frame
  (locals args return-to))

(struct unison-frame-mark unison-frame
  (args abilities handlers))

(define-syntax (define-builtin-link stx)
  (syntax-case stx ()
    [(_ name)
     (identifier? #'name)
     (let* ([sym (syntax-e #'name)]
            [txt (symbol->string sym)]
            [dname (datum->syntax stx
                     (string->symbol
                       (string-append
                         "builtin-" txt ":termlink"))
                     #'name)])
       (quasisyntax/loc stx
         (define #,dname
           (unison-termlink-builtin #,(datum->syntax stx txt)))))]))

(define-syntax (declare-builtin-link stx)
  (syntax-case stx ()
    [(_ name)
     (identifier? #'name)
     (let* ([sym (syntax-e #'name)]
            [txt (symbol->string sym)]
            [dname (datum->syntax stx
                     (string->symbol
                       (string-append txt ":termlink")))])
       (quasisyntax/loc stx
         (declare-function-link name #,dname)))]))

(define (partial-app f . args) (unison-closure f args))

; Option a
(define none (sum 0))

; a -> Option a
(define (some a) (sum 1 a))

; Option a -> Bool
(define (some? option) (eq? 1 (unison-sum-tag option)))

; Option a -> Bool
(define (none? option) (eq? 0 (unison-sum-tag option)))

; Option a -> a (or #f)
(define (option-get option)
  (if
   (some? option)
   (car (unison-sum-fields option))
   (raise "Cannot get the value of an empty option ")))

; #<void> works as well
; Unit
(define sum-unit (sum 0))

; Booleans are represented as numbers
(define sum-false 0)
(define sum-true 1)

(define (bool b) (if b 1 0))

(define (char c) (char->integer c))

(define (ord o)
  (cond
    [(eq? o '<) 0]
    [(eq? o '=) 1]
    [(eq? o '>) 2]))

; a -> Either b a
(define (right a) (sum 1 a))

; b -> Either b a
(define (left b) (sum 0 b))

; Either a b -> Boolean
(define (right? either) (eq? 1 (unison-sum-tag either)))

; Either a b -> Boolean
(define (left? either) (eq? 0 (unison-sum-tag either)))

; Either a b -> a | b
(define (either-get either) (car (unison-sum-fields either)))

; a -> Any
(define builtin-any:typelink (unison-typelink-builtin "Any"))
(define unison-any-any:tag 0)
(define (unison-any-any x)
  (data builtin-any:typelink unison-any-any:tag x))

(define builtin-boolean:typelink (unison-typelink-builtin "Boolean"))
(define unison-boolean-true:tag 1)
(define unison-boolean-false:tag 0)
(define unison-boolean-true
  (data builtin-boolean:typelink unison-boolean-true:tag))
(define unison-boolean-false
  (data builtin-boolean:typelink unison-boolean-false:tag))

(define builtin-bytes:typelink (unison-typelink-builtin "Bytes"))
(define builtin-char:typelink (unison-typelink-builtin "Char"))
(define builtin-code:typelink (unison-typelink-builtin "Code"))
(define builtin-float:typelink (unison-typelink-builtin "Float"))
(define builtin-int:typelink (unison-typelink-builtin "Int"))
(define builtin-mvar:typelink (unison-typelink-builtin "MVar"))
(define builtin-nat:typelink (unison-typelink-builtin "Nat"))
(define builtin-pattern:typelink (unison-typelink-builtin "Pattern"))
(define builtin-promise:typelink (unison-typelink-builtin "Promise"))
(define builtin-sequence:typelink (unison-typelink-builtin "Sequence"))
(define builtin-socket:typelink (unison-typelink-builtin "Socket"))
(define builtin-text:typelink (unison-typelink-builtin "Text"))
(define builtin-tls:typelink (unison-typelink-builtin "Tls"))
(define builtin-timespec:typelink (unison-typelink-builtin "TimeSpec"))
(define builtin-threadid:typelink (unison-typelink-builtin "ThreadId"))
(define builtin-value:typelink (unison-typelink-builtin "Value"))
(define builtin-udpsocket:typelink (unison-typelink-builtin "UDPSocket"))
(define builtin-listensocket:typelink (unison-typelink-builtin "ListenSocket"))
(define builtin-clientsockaddr:typelink (unison-typelink-builtin "ClientSockAddr"))

(define builtin-crypto.hashalgorithm:typelink
  (unison-typelink-builtin "crypto.HashAlgorithm"))
(define builtin-char.class:typelink
  (unison-typelink-builtin "Char.Class"))
(define builtin-immutablearray:typelink
  (unison-typelink-builtin "ImmutableArray"))
(define builtin-immutablebytearray:typelink
  (unison-typelink-builtin "ImmutableByteArray"))
(define builtin-mutablearray:typelink
  (unison-typelink-builtin "MutableArray"))
(define builtin-mutablebytearray:typelink
  (unison-typelink-builtin "MutableArray"))
(define builtin-processhandle:typelink
  (unison-typelink-builtin "ProcessHandle"))
(define builtin-ref.ticket:typelink
  (unison-typelink-builtin "Ref.Ticket"))
(define builtin-tls.cipher:typelink
  (unison-typelink-builtin "Tls.Cipher"))
(define builtin-tls.clientconfig:typelink
  (unison-typelink-builtin "Tls.ClientConfig"))
(define builtin-tls.privatekey:typelink
  (unison-typelink-builtin "Tls.PrivateKey"))
(define builtin-tls.serverconfig:typelink
  (unison-typelink-builtin "Tls.ServerConfig"))
(define builtin-tls.signedcert:typelink
  (unison-typelink-builtin "Tls.SignedCert"))
(define builtin-tls.version:typelink
  (unison-typelink-builtin "Tls.Version"))

; Type -> Text -> Any -> Failure
(define (failure typeLink msg any)
  (sum 0 typeLink msg any))

; Type -> Text -> a -> (type, text, a) + b
(define (exception typeLink msg a)
  (failure typeLink msg a))

; A counter for internally numbering declared data, so that the
; entire reference doesn't need to be stored in every data record.
(define next-data-number 0)
(define (fresh-data-number)
  (let ([n next-data-number])
    (set! next-data-number (+ n 1))
    n))

; maps hashes of declared unison data to their internal numberings
(define data-hash-numberings (make-hash))

; maps internal numberings of declared unison data to their hashes
(define data-number-hashes (make-hash))

; Adds a hash to the known set of data types, allocating an
; internal number for it.
(define (declare-unison-data-hash bs)
  (let ([n (fresh-data-number)])
    (hash-set! data-hash-numberings bs n)
    (hash-set! data-number-hashes n bs)))

(define (data-hash->number bs)
  (hash-ref data-hash-numberings bs))

(define (data-number->hash n)
  (hash-ref data-number-hashes n))

(define function-associations (make-hash))

(define (declare-function-link f ln)
  (hash-set! function-associations f ln))

(define (lookup-function-link f)
  (hash-ref function-associations f))

(define code-associations (make-hash))

(define (declare-code hs co)
  (unless (hash-has-key? code-associations hs)
    (hash-set! code-associations hs co)))

(define (lookup-code hs)
  (let ([mco (hash-ref code-associations hs #f)])
    (if (eq? mco #f)
      (sum 0)
      (sum 1 mco))))

(define (have-code? hs)
  (hash-has-key? code-associations hs))

(define (unison-tuple->list t)
  (let ([fs (unison-data-fields t)])
    (cond
      [(null? fs) '()]
      [(= 2 (length fs))
       (cons (car fs) (unison-tuple->list (cadr fs)))]
      [else
        (raise "unison-tuple->list: unexpected value")])))

(define (unison-pair->cons t)
  (match t
    [(unison-data _ _ (list x (unison-data _ _ (list y _))))
     (cons x y)]
    [else
     (raise "unison-pair->cons: unexpected value")]))

(define (hash-string hs)
  (string-append
    "#"
    (bytevector->base32-string hs #:alphabet 'hex)))

(define (ix-string #:sep [sep "."] i)
  (if (= i 0)
    ""
    (string-append sep (number->string i))))

(define (clip short s) (if short (substring s 0 8) s))

(define (typelink->string ln [short #f])
  (match ln
    [(unison-typelink-builtin name)
     (string-append "##" name)]
    [(unison-typelink-derived hs i)
     (string-append (clip short (hash-string hs)) (ix-string i))]))

(define (groupref->string gr [short #f])
  (match gr
    [(unison-groupref-builtin name)
     (string-append "##" name)]
    [(unison-groupref-derived hs i l)
     (string-append
       (clip short (hash-string hs))
       (ix-string i)
       (ix-string #:sep "-" l))]))

(define (termlink->string ln [short #f])
  (define (clip s) (if short (substring s 0 8) s))

  (match ln
    [(unison-termlink-builtin name)
     (string-append "##" name)]
    [(unison-termlink-derived hs i)
     (string-append (clip (hash-string hs)) (ix-string i))]
    [(unison-termlink-con rf t)
     (string-append
       (typelink->string rf short) "#" (number->string t))]))

(define (groupref->termlink gr)
  (match gr
    [(unison-groupref-builtin name)
     (unison-termlink-builtin name)]
    [(unison-groupref-derived hs i _)
     (unison-termlink-derived hs i)]))

(define (termlink->groupref ln l)
  (match ln
    [#f #f]
    [(unison-termlink-builtin name)
     (unison-groupref-builtin name)]
    [(unison-termlink-derived hs i)
     (unison-groupref-derived hs i l)]
    [(unison-termlink-con r i)
     (raise-argument-error
       'termlink->groupref
       "builtin or derived link"
       ln)]))

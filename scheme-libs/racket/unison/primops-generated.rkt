; This library implements primops that require unison-generated
; code as a basis. It imports the boot-generated library which is
; itself allowed to import the ordinary primops module for running
; unison code. Then the surface code can import _both_ sets of
; primop moduels for the purpose of wrapping them into surface unison
; code.
#!racket/base
(require (except-in racket false true unit any)
         racket/vector
         racket/hash
         unison/boot
         unison/boot-generated
         (only-in unison/bytevector bytevector->base32-string)
         unison/data
         unison/data-info
         unison/chunked-seq
         (for-syntax racket/base unison/data-info))

(provide
  builtin-Value.value
  builtin-Value.value:termlink
  builtin-Value.reflect
  builtin-Value.reflect:termlink
  builtin-Code.isMissing
  builtin-Code.isMissing:termlink
  builtin-Code.lookup
  builtin-Code.lookup:termlink
  builtin-validateSandboxed
  builtin-validateSandboxed:termlink
  builtin-Value.validateSandboxed
  builtin-Value.validateSandboxed:termlink
  builtin-sandboxLinks
  builtin-sandboxLinks:termlink

  builtin-Code.dependencies:termlink
  builtin-Code.deserialize:termlink
  builtin-Code.serialize:termlink
  builtin-Code.validateLinks:termlink
  builtin-Value.dependencies:termlink
  builtin-Value.deserialize:termlink
  builtin-Value.serialize:termlink
  builtin-crypto.hash:termlink
  builtin-crypto.hmac:termlink

  builtin-Value.load
  builtin-Value.load:termlink
  builtin-Code.cache_
  builtin-Code.cache_:termlink

  ; some exports of internal machinery for use elsewhere
  reify-value
  reflect-value
  termlink->name

  add-runtime-code
  build-intermediate-module
  build-runtime-module
  termlink->proc)

(define-builtin-link Code.dependencies)
(define-builtin-link Code.deserialize)
(define-builtin-link Code.serialize)
(define-builtin-link Code.validateLinks)
(define-builtin-link Value.dependencies)
(define-builtin-link Value.deserialize)
(define-builtin-link Value.serialize)
(define-builtin-link crypto.hash)
(define-builtin-link crypto.hmac)

(define (chunked-list->list cl)
  (vector->list (chunked-list->vector cl)))

(define (list->chunked-list l)
  (vector->chunked-list (list->vector l)))

(define (assemble-cases hd sc cs)
  (cond
    [(equal? hd 'cond) `(cond ,@cs)]
    [else `(,hd ,sc ,@cs)]))

(define (decode-term tm)
  (match tm
    [(unison-data _ t (list tms))
     #:when (= t ref-schemeterm-sexpr:tag)
     (map decode-term (chunked-list->list tms))]
    [(unison-data _ t (list as h tms))
     #:when (= t ref-schemeterm-handle:tag)
     `(handle
        ,(map text->ident (chunked-list->list as))
        ,(text->ident h)
        ,@(map decode-term (chunked-list->list tms)))]
    [(unison-data _ t (list hd sc cs))
     #:when (= t ref-schemeterm-cases:tag)
     (assemble-cases
       (text->ident hd)
       (decode-term sc)
       (map decode-term (chunked-list->list cs)))]
    [(unison-data _ t (list hd bs bd))
     #:when (= t ref-schemeterm-binds:tag)
     `(,(text->ident hd)
        ,(map decode-binding (chunked-list->list bs))
        ,(decode-term bd))]
    [(unison-data _ t (list tx))
     #:when (= t ref-schemeterm-ident:tag)
     (text->ident tx)]
    [(unison-data _ t (list tx))
     #:when (= t ref-schemeterm-string:tag)
     (chunked-string->string tx)]
    [(unison-data _ t (list tx))
     #:when (= t ref-schemeterm-symbol:tag)
     `(quote ,(text->ident tx))]
    [(unison-data _ t (list ns))
     #:when (= t ref-schemeterm-bytevec:tag)
     (list->bytes (chunked-list->list ns))]
    [else
      (raise (format "decode-term: unimplemented case: ~a" tm))]))

(define (decode-binding bn)
  (data-case bn
    [0 (nm rest)
      (data-case rest
        [0 (tm nil) (list (text->ident nm) (decode-term tm))])]
    [else
      (raise
        (format "decode-binding: unimplemented case: ~a" bn))]))

; This decodes the internal unison SchemeIntermed structure for
; representing generated declarations of intermediate code. The
; structure is just a pair of a name and a SchemeTerm representing
; the code.
(define (decode-intermediate im)
  (match im
    [(unison-data _ t (list name tm))
     #:when (= t ref-schemeintermed-interdef:tag)
     `(define ,(text->ident name #:suffix ":code")
        ,(decode-term tm))]
    [else
     (raise-argument-error
       'decode-intermediate
       "scheme-intermediate"
       im)]))

(define (decode-hints hs)
  (define (hint->sym t)
    (cond
      [(= t ref-defnhint-internal:tag) 'internal]
      [(= t ref-defnhint-genlink:tag) 'gen-link]
      [(= t ref-defnhint-nolinkdecl:tag) 'no-link-decl]))

  (for/fold ([def 'define-unison] [out '()]) ([h hs])
    (match h
      [(unison-data _ t (list))
       #:when (= t ref-defnhint-builtin:tag)
       (values 'define-unison-builtin out)]
      [(unison-data _ t (list))
       (values def (cons (hint->sym t) out))])))

(define (decode-local lo)
  (match lo
    [(unison-data _ t (list))
     #:when (= t ref-optional-none:tag)
     0]
    [(unison-data _ t (list n))
     #:when (= t ref-optional-some:tag)
     n]))

(define (decode-syntax dfn)
  (match dfn
    [(unison-data _ t (list nm lo hs vs bd))
     #:when (= t ref-schemedefn-define:tag)
     (let-values
       ([(head) (map text->ident
                  (cons nm (chunked-list->list vs)))]
        [(ln) (decode-local lo)]
        [(def hints) (decode-hints (chunked-list->list hs))]
        [(body) (decode-term bd)])
       (if (null? hints)
         (list def '#:local ln head body)
         (list def '#:local ln '#:hints hints head body)))]
    [(unison-data _ t (list nm hs bd))
     #:when (= t ref-schemedefn-defineval:tag)
     (let-values
       ([(head) (text->ident nm)]
        [(def hints) (decode-hints (chunked-list->list hs))]
        [(body) (decode-term bd)])
       (list def '#:hints (cons 'value hints) (list head) body))]
    [(unison-data _ t (list nm bd))
     #:when (= t ref-schemedefn-alias:tag)
     (list 'define (text->ident nm) (decode-term bd))]
    [else
      (raise (format "decode-syntax: unimplemented case: ~a" dfn))]))

(define (string->char st)
  (cond
    [(< (string-length st) 3) #f]
    [(> (string-length st) 3) #f]
    [(equal? (substring st 0 2) "#\\") (string-ref st 2)]
    [else #f]))

(define (text->linkname tx)
  (let* ([st (chunked-string->string tx)])
    (string->symbol (string-append st ":typelink"))))

(define (text->ident tx #:suffix [suffix ""])
  (let* ([st (chunked-string->string tx)]
         [n (string->number st)]
         [c (string->char st)])
    (cond
      [(equal? st "#f") #f]
      [(equal? st "#t") #t]
      [c c]
      [n n]
      [else (string->symbol (string-append st suffix))])))

(define (decode-ref rf)
  (match rf
    [(unison-data r t (list name))
     #:when (= t ref-reference-builtin:tag)
     (sum 0 (chunked-string->string name))]
    [(unison-data r t (list id))
     #:when (= t ref-reference-derived:tag)
     (data-case id
       [0 (bs i) (sum 1 bs i)])]))

(define-syntax make-termlink-decoder
  (lambda (stx)
    (syntax-case stx ()
      [(_)
       #`(lambda (tl)
           (data-case tl
             ; TODO: generate this programatically
             [0 (rf i) (sum 0 rf i)] ; con case
             [1 (rf) (sum 1 rf)]))]))) ; ref case

(define decode-termlink (make-termlink-decoder))

(define (termlink-ref tl)
  (sum-case (decode-termlink tl)
    [0 (rf i) (raise
                (format
                  "termlink-ref: unexpected constructor reference"
                  (describe-value tl)))]
    [1 (rf) rf]))

(define (decode-group-ref gr0)
  (match (group-ref-ident gr0)
    [(unison-data _ t (list name))
     #:when (= t ref-schemeterm-ident:tag)
     name]
    [else
     (raise
       (format
         "decode-group-ref: unimplemented data case: ~a"
         (describe-value gr0)))]))

(define (group-ref-sym gr)
  (string->symbol
    (chunked-string->string
      (decode-group-ref gr))))

(define (termlink->name tl)
  (match tl
    [(unison-termlink-con r i)
     (raise "termlink->name: data constructor")]
    [(unison-termlink-builtin name)
     (string-append "builtin-" name)]
    [(unison-termlink-derived bs i)
     (let* ([hs (bytevector->base32-string bs #:alphabet 'hex)]
            [tm (string-trim hs "=" #:repeat? #t)]
            [po (if (= i 0) "" (string-append "." (number->string i)))])
       (string->symbol
         (string-append "ref-" tm po)))]))

(define (ref-bytes r)
  (sum-case (decode-ref r)
    [0 (tx) (raise (string-append "ref-bytes: builtin ref: " tx))]
    [1 (bs i) bs]))

(define (termlink-bytes tl)
  (match tl
    [(unison-termlink-derived bs i) bs]
    [(unison-termlink-builtin name)
     (raise (string-append "termlink-bytes: builtin ref: " name))]
    [(unison-termlink-con r i)
     (raise (string-append
              "termlink-bytes: called with constructor link"))]))

(define (group-reference gr)
  (data-case gr
    [0 (r _) r]))

(define runtime-namespace
  (let ([ns (variable-reference->namespace (#%variable-reference))])
    (namespace-require ''#%kernel ns)
    ns))

(define runtime-module-term-map (make-hash))
(define runtime-module-type-map (make-hash))

(define (reflect-derived bs i)
  (data ref-reference:typelink ref-reference-derived:tag
    (data ref-id:typelink ref-id-id:tag bs i)))

(define (function->groupref f)
  (reflect-groupref (unison-closure-ref (build-closure f))))

(define (link->groupref ln)
  (match ln
    [(unison-termlink-derived h i)
     (ref-groupref-group
       (ref-reference-derived
         (ref-id-id h i))
       0)]
    [(unison-termlink-builtin name)
     (ref-groupref-group
       (ref-reference-builtin (string->chunked-string name))
       0)]
    [else (raise "link->groupref: con case")]))

(define (reify-vlit vl)
  (match vl
    [(unison-data _ t (list l))
     (cond
       [(= t ref-vlit-bytes:tag) l]
       [(= t ref-vlit-char:tag) l]
       [(= t ref-vlit-bytearray:tag) l]
       [(= t ref-vlit-text:tag) l]
       [(= t ref-vlit-termlink:tag) (referent->termlink l)]
       [(= t ref-vlit-typelink:tag) (reference->typelink l)]
       [(= t ref-vlit-float:tag) l]
       [(= t ref-vlit-pos:tag) l]
       [(= t ref-vlit-neg:tag) (- l)]
       [(= t ref-vlit-quote:tag) (unison-quote l)]
       [(= t ref-vlit-code:tag) (unison-code l)]
       [(= t ref-vlit-array:tag) (vector-map reify-value l)]
       [(= t ref-vlit-seq:tag)
        ; TODO: better map over chunked list
        (vector->chunked-list
          (vector-map reify-value (chunked-list->vector l)))]
       [else
         (raise (format "decode-vlit: unimplemented case: !a" vl))])]))

(define (reify-handlers hs)
  (for/list ([h (chunked-list->list hs)])
    (match (unison-pair->cons h)
      [(cons r h)
       (cons (reference->typelink r)
             (reify-value h))])))

(define (reflect-handlers hs)
  (list->chunked-list
    (for/list ([h hs])
      (match h
        [(cons r h)
         (unison-tuple
           (typelink->reference r)
           (reflect-value h))]))))

(define (reify-groupref gr0)
  (match gr0
    [(unison-data _ t (list r i))
     #:when (= t ref-groupref-group:tag)
     (cons (reference->typelink r) i)]))

(define (parse-continuation orig k0 vs0)
  (let rec ([k k0] [vs vs0] [frames '()])
    (match k
      [(unison-data _ t (list))
       #:when (= t ref-cont-empty:tag)
       (unison-cont-reflected (reverse frames))]
      [(unison-data _ t (list l a gr0 k))
       #:when (= t ref-cont-push:tag)
       (cond
         [(>= (length vs) (+ l a))
          (let*-values
            ([(locals int) (split-at vs l)]
             [(args rest) (split-at int a)]
             [(gr) (reify-groupref gr0)]
             [(fm) (unison-frame-push locals args gr)])
            (rec k rest (cons fm frames)))]
         [else
          (raise
            (make-exn:bug
              "reify-value: malformed continuation"
              orig))])]
      [(unison-data _ t (list a rs0 de0 k))
       #:when (= t ref-cont-mark:tag)
       (cond
         [(>= (length vs) a)
          (let*-values
            ([(args rest) (split-at vs a)]
             [(rs) (map reference->termlink (chunked-list->list rs0))]
             [(hs) (reify-handlers de0)]
             [(fm) (unison-frame-mark args rs hs)])
            (rec k rest (cons fm frames)))]
         [else
          (raise
            (make-exn:bug
              "reify-value: malformed continuation"
              orig))])])))

(define (reify-value v)
  (match v
    [(unison-data _ t (list rf rt bs0))
     #:when (= t ref-value-data:tag)
     (let ([bs (map reify-value (chunked-list->list bs0))]
           [tl (reference->typelink rf)])
       (cond
         [(equal? tl builtin-boolean:typelink)
          (cond
            [(not (null? bs))
             (raise
               (make-exn:bug
                 "reify-value: boolean with arguments"
                 bs0))]
            [(= rt 0) #f]
            [(= rt 1) #t]
            [else
             (raise
               (make-exn:bug
                 "reify-value: unknown boolean tag"
                 rt))])]
         [else (make-data tl rt bs)]))]
    [(unison-data _ t (list gr bs0))
     #:when (= t ref-value-partial:tag)
     (let ([bs (map reify-value (chunked-list->list bs0))]
           [proc (build-closure (resolve-proc gr))])
       (struct-copy unison-closure proc [env bs]))]
    [(unison-data _ t (list vl))
     #:when (= t ref-value-vlit:tag)
     (reify-vlit vl)]
    [(unison-data _ t (list vs0 k))
     #:when (= t ref-value-cont:tag)
     (parse-continuation v k
       (map reify-value (chunked-list->list vs0)))]
    [(unison-data r t fs)
     (raise
       (make-exn:bug
         "reify-value: unrecognized tag"
         ref-unit-unit))]
    [else
     (raise
       (make-exn:bug "reify-value: unrecognized value" v))]))

(define (reflect-typelink tl)
  (match tl
    [(unison-typelink-builtin name)
     (ref-reference-builtin
       (string->chunked-string name))]
    [(unison-typelink-derived h i)
     (ref-reference-derived (ref-id-id h i))]))

(define (reflect-termlink tl)
  (match tl
    [(unison-termlink-con r i)
     (ref-referent-con (reflect-typelink r) i)]
    [(unison-termlink-builtin name)
     (ref-referent-def
       (ref-reference-builtin
         (string->chunked-string name)))]
    [(unison-termlink-derived h i)
     (ref-referent-def
       (ref-reference-derived
         (ref-id-id h i)))]))

(define (number-reference n)
  (cond
    [(exact-nonnegative-integer? n)
     (ref-reference-builtin (string->chunked-string "Nat"))]
    [(exact-integer? n)
     (ref-reference-builtin (string->chunked-string "Int"))]
    [else
      (ref-reference-builtin (string->chunked-string "Float"))]))

(define (reflect-groupref gr)
  (match gr
    [(unison-groupref-derived h i l)
     (ref-groupref-group
       (ref-reference-derived
         (ref-id-id h i))
       l)]
    [(unison-groupref-builtin name)
     (ref-groupref-group
       (ref-reference-builtin (string->chunked-string name))
       0)]))

(define (reflect-value v)
  (match v
    [(? boolean?)
     (ref-value-data
       (reflect-typelink builtin-boolean:typelink)
       (if v 1 0) ; boolean pseudo-data tags
       empty-chunked-list)]
    [(? exact-nonnegative-integer?)
     (ref-value-vlit (ref-vlit-pos v))]
    [(? exact-integer?)
     (ref-value-vlit (ref-vlit-neg (- v)))]
    [(? inexact-real?)
     (ref-value-vlit (ref-vlit-float v))]
    [(? char?)
     (ref-value-vlit (ref-vlit-char v))]
    [(? chunked-bytes?)
     (ref-value-vlit (ref-vlit-bytes v))]
    [(? bytes?)
     (ref-value-vlit (ref-vlit-bytearray v))]
    [(? vector?)
     (ref-value-vlit
       (ref-vlit-array
         (vector-map reflect-value v)))]
    [(? chunked-string?)
     (ref-value-vlit (ref-vlit-text v))]
    ; TODO: better map over chunked lists
    [(? chunked-list?)
     (ref-value-vlit
       (ref-vlit-seq
         (list->chunked-list
           (map reflect-value (chunked-list->list v)))))]
    [(? unison-termlink?)
     (ref-value-vlit (ref-vlit-termlink (reflect-termlink v)))]
    [(? unison-typelink?)
     (ref-value-vlit (ref-vlit-typelink (reflect-typelink v)))]
    [(unison-code sg) (ref-value-vlit (ref-vlit-code sg))]
    [(unison-quote q) (ref-value-vlit (ref-vlit-quote q))]
    [(unison-cont-reflected frames0)
     (for/foldr ([k ref-cont-empty]
                 [vs '()]
                 #:result
                 (ref-value-cont
                   (list->chunked-list (map reflect-value vs))
                   k))
                ([frame frames0])
       (match frame
         [(unison-frame-push locals args return-to)
          (values
            (ref-cont-push
              (length locals)
              (length args)
              (reflect-groupref return-to)
              k)
            (append locals args vs))]
         [(unison-frame-mark args refs hs)
          (values
            (ref-cont-mark
              (length args)
              (map typelink->reference refs)
              (reflect-handlers hs))
            (append args vs))]))]
    [(unison-closure gr f as)
     (ref-value-partial
       (reflect-groupref gr)
       (list->chunked-list (map reflect-value as)))]
    [(? procedure?) (reflect-value (build-closure v))]
    [(unison-data rf t fs)
     (ref-value-data
       (reflect-typelink rf)
       t
       (list->chunked-list (map reflect-value fs)))]))

(define (check-sandbox-ok ok l)
  (remove* ok (check-sandbox l)))

(define (sandbox-scheme-value ok v)
  (match v
    [(? chunked-list?)
     (for/fold ([acc '()]) ([e (in-chunked-list v)])
       (append (sandbox-scheme-value ok e) acc))]
    [(unison-closure gr f as)
     (define link (groupref->termlink gr))
     (for/fold ([acc (check-sandbox-ok ok link)]) ([a (in-list as)])
       (append (sandbox-scheme-value ok a) acc))]
    [(? procedure?) (sandbox-scheme-value ok (build-closure v))]
    [(unison-data rf t fs)
     (for/fold ([acc '()]) ([e (in-list fs)])
       (append (sandbox-scheme-value ok e) acc))]
    [else '()]))

(define (check-known l acc)
  (if (need-code? l) (cons l acc) acc))

; check sandboxing information for an internal.runtime.Value
(define (sandbox-value ok v)
  (for/fold
    ([sdbx '()]
     [unkn '()]

     #:result
     (if (null? unkn)
       (ref-either-right (list->chunked-list sdbx))
       (ref-either-left (list->chunked-list unkn))))

    ([r (in-chunked-list (value-term-dependencies v))])

    (let ([l (reference->termlink r)])
      (values
        (append (check-sandbox-ok ok l) sdbx)
        (check-known l unkn)))))

; check sandboxing information for a reflection.Value
(define (sandbox-quoted ok qv)
  (match qv
    [(unison-quote v) (sandbox-value ok v)]))

; replacment for Value.unsafeValue : a -> Value
(define-unison-builtin
  (builtin-Value.reflect v)
  (reflect-value v))

(define-unison-builtin
  (builtin-Value.value v)
  (let ([rv (reflect-value v)])
    (unison-quote rv)))

(define (ufst utup)
  (data-case utup
    [0 (fst _) fst]))

(define (usnd utup)
  (data-case utup
    [0 (_ rest)
      (data-case rest
        [0 (snd _) snd])]))

(define (splat-upair utup)
  (data-case utup
    [0 (fst rest)
      (data-case rest
        [0 (snd nil)
          (values fst snd)])]))

(define (typelink-deps code)
  (group-type-dependencies
    (list->chunked-list
      (map unison-code-rep code))))

(define (typelink-defns-code links)
  (map decode-syntax
    (chunked-list->list
      (gen-typelink-defns links))))

(define (gen-code-decl r)
  (define linkstr (chunked-string->string (ref-typelink-name r)))
  (define name:link
    (string->symbol (string-replace linkstr "typelink" "termlink")))
  (define name:code
    (string->symbol (string-replace linkstr "typelink" "code")))

  `(declare-code ,name:link (unison-code ,name:code)))

; Given a termlink, code pair, generates associated definition
; and declaration code. Returns multiple results.
;
; This is the runtime loading version. It isn't necessary to generate
; code related definitions, because we already have the code values
; to add directly to the cache.
(define (gen-code:runtime arities tl co)
  (match tl
    [(unison-termlink-derived bs i)
     (define sg (unison-code-rep co))
     (define r (reflect-derived bs i))
     (define ln (decode-syntax (gen-link-def r)))
     (define ds (chunked-list->list (gen-scheme arities r sg)))
     (define dc (decode-term (gen-link-decl r)))

     (values ln dc (map decode-syntax ds))]
    [else
     (raise-argument-error
       'gen-code:runtime
       "unison-termlink-derived?"
       tl)]))

; Given a termlink, code pair, generates associated definition
; and declaration code. Returns multiple results.
;
; This is the version for compiling to intermediate code. It generates
; code declarations that will recreate the code values in the
; compiled executable.
(define (gen-code:intermed arities tl co)
  (match tl
    [(unison-termlink-derived bs i)
     (define sg (unison-code-rep co))
     (define r (reflect-derived bs i))
     (define ln (decode-syntax (gen-link-def r)))
     (define dc (decode-term (gen-link-decl r)))
     (define cv (decode-intermediate (gen-code-value r sg)))
     (define cd (gen-code-decl r))
     (define ds (chunked-list->list (gen-scheme arities r sg)))

     (values ln dc cv cd (map decode-syntax ds))]
    [else
     (raise-argument-error
       'gen-code:intermed
       "unison-termlink-derived?"
       tl)]))

; Converts a link->code map into an appropriately sorted list
; for code generation. It's necessary to topologically sort
; the code so that values occur after the things they reference.
(define (codemap->link-order defs)
  (define input
    (for/list ([(tl co) defs])
      (unison-tuple
        (termlink->reference tl)
        (unison-code-rep co))))

  (define result (topsort-code-refs (list->chunked-list input)))

  (for/list ([r (in-chunked-list result)])
    (reference->termlink r)))

; Given a list of termlink, code pairs, returns multiple lists
; of definitions and declarations. The lists are returned as
; multiple results, each one containing a particular type of
; definition.
;
; This is the version for compiling to runtime code.
(define (gen-codes:runtime arities defs)
  (for/lists (lndefs lndecs dfns)
             ([tl (codemap->link-order defs)])
    (gen-code:runtime arities tl (hash-ref defs tl))))

; Given a list of termlink, code pairs, returns multiple lists
; of definitions and declarations. The lists are returned as
; multiple results, each one containing a particular type of
; definition.
;
; This is the version for compiling to intermediate code.
(define (gen-codes:intermed arities defs)
  (for/lists (lndefs lndecs codefs codecls dfns)
             ([tl (codemap->link-order defs)])
      (gen-code:intermed arities tl (hash-ref defs tl))))

(define (flatten ls)
  (cond
    [(null? ls) '()]
    [else (append (car ls) (flatten (cdr ls)))]))

(define module-count (box 0))

(define (fresh-module-name)
  (let* ([n (unbox module-count)]
         [sn (+ n 1)])
    (if (box-cas! module-count n sn)
      (string-append "runtime-module-" (number->string n))
      (fresh-module-name))))

(define (generate-module-name links)
  (string->symbol (fresh-module-name)))

(define (register-code udefs)
  (for ([(ln co) udefs])
    (declare-code ln co)))

(define (runtime-code-loaded? link)
  (hash-has-key? runtime-module-term-map (termlink-bytes link)))

(define (add-module-term-associations links mname)
  (for ([link links])
    (define bs (termlink-bytes link))
    (unless (hash-has-key? runtime-module-term-map bs)
      (hash-set! runtime-module-term-map bs mname))))

(define (add-module-type-associations links mname)
  (for ([link links])
    (unless (hash-has-key? runtime-module-type-map link)
      (hash-set! runtime-module-type-map link mname))))

(define ((assoc-raise name l))
  (raise-argument-error name "declared link" l))

(define (termlink->module link
                          [default (assoc-raise
                                     'termlink->module
                                     (describe-value link))])
  (termbytes->module (termlink-bytes link) default))

(define (termbytes->module bs
                           [default (assoc-raise
                                      'termbytes->module
                                      (describe-hash bs))])
  (hash-ref runtime-module-term-map bs default))

; Resolves the module in which a typelink is declared. Using a
; canonical typelink is important for abilities, because the
; continuation mechanism uses eq? to compare them. This should
; only be a concern for code, though.
(define (typelink->module link
                          [default (assoc-raise
                                     'module-type-association
                                     (describe-value link))])
  (hash-ref runtime-module-type-map link default))

(define (need-code? l)
  (define ln (if (unison-data? l) (reference->termlink l) l))
  (and (unison-termlink-derived? ln) (not (have-code? ln))))

(define (need-code-loaded? l)
  (define ln (if (unison-data? l) (reference->termlink l) l))
  (and (unison-termlink-derived? ln) (not (runtime-code-loaded? ln))))

(define (have-code-loaded? ln)
  (and (unison-termlink-derived? ln) (runtime-code-loaded? ln)))

(define (need-typelink? l)
  (let ([ln (if (unison-data? l) (reference->typelink l) l)])
    (not (hash-has-key? runtime-module-type-map ln))))

(define (resolve-builtin nm)
  (dynamic-require 'unison/primops nm))

(define (termlink->proc tl)
  (match tl
    [(unison-termlink-derived bs i)
     (let ([mname (hash-ref runtime-module-term-map bs)])
       (parameterize ([current-namespace runtime-namespace])
         (dynamic-require `(quote ,mname) (termlink->name tl))))]
    [(unison-termlink-builtin name)
     (let ([mname (string->symbol (string-append "builtin-" name))])
       (parameterize ([current-namespace runtime-namespace])
         (resolve-builtin mname)))]))

(define (resolve-proc gr)
  (sum-case (decode-ref (group-reference gr))
    [0 (tx)
     (parameterize ([current-namespace runtime-namespace])
       (resolve-builtin
         (string->symbol (string-append "builtin-" tx))))]
    [1 (bs i)
     (let ([sym (group-ref-sym gr)]
           [mname (termbytes->module bs)])
       (parameterize ([current-namespace runtime-namespace])
         (dynamic-require `(quote ,mname) sym)))]))

; Straight-line module builder given intermediate definitions.
; This expects to receive a list of termlink, code pairs, and
; generates a scheme module that contains the corresponding
; definitions.
(define (build-intermediate-module #:profile [profile? #f] primary dfns0)
  (define udefs
    (for/hash ([p (in-chunked-list dfns0)]
               #:when (need-code-loaded? (ufst p)))
      (splat-upair p)))
  (define-values (tmlinks codes arities)
    (for/lists (ts cs as)
               ([(tl co) udefs])
               (values tl co (arity-tuple tl co))))

  (define pname (termlink->name primary))
  (define tylinks (typelink-deps codes))

  (define-values
    (lndefs lndecs codefs codecls dfns)
    (gen-codes:intermed (list->chunked-list arities) udefs))

  `((require unison/boot
             unison/data
             unison/data-info
             unison/primops
             unison/primops-generated
             unison/builtin-generated
             ,@(if profile? '(profile profile/render-text) '()))

    ,@(typelink-defns-code tylinks)

    ; termlink definitions
    ,@lndefs

    ; procedure definitions
    ,@(flatten dfns)

    ; code definitions
    ,@codefs

    ; code declarations
    ,@codecls

    ,(if profile?
       `(profile
          (handle [ref-exception] top-exn-handler (,pname #f))
          #:threads #t
          #:periodic-renderer (list 60.0 render))
       `(handle [ref-exception] top-exn-handler (,pname #f)))))

(define (extra-requires tyrefs tmrefs)
  (define tmreqs
    (for/list ([l tmrefs]
               #:when (unison-termlink-derived? l))
      (termlink->module l)))

  (define tyreqs
    (for/list ([l (map reference->typelink tyrefs)]
               #:when (unison-typelink-derived? l))
      (typelink->module l #f)))

  (remove #f (remove-duplicates (append tmreqs tyreqs))))


(define (build-runtime-module mname reqs tylinks tmlinks defs)
  (define (provided-tylink r)
    (string->symbol
      (chunked-string->string
        (ref-typelink-name r))))
  (define tynames (map provided-tylink (chunked-list->list tylinks)))
  (define tmnames (map termlink->name tmlinks))
  `(module ,mname racket/base
     (require unison/boot
              unison/data
              unison/data-info
              unison/primops
              unison/primops-generated
              unison/builtin-generated
              ,@(map (lambda (s) `(quote ,s)) reqs))

     (provide
       ,@tynames
       ,@tmnames)

     ,@(typelink-defns-code tylinks)

     ,@defs))

(define (add-runtime-module mname reqs tylinks tmlinks defs)
  (eval (build-runtime-module mname reqs tylinks tmlinks defs)
        runtime-namespace))

(define (code-dependencies co)
  (map reference->termlink
    (chunked-list->list
      (group-term-dependencies
        (unison-code-rep co)))))

; Extracts the main arity of a code value. Only the main entry
; is called from other combinators.
(define (code-arity co) (group-arity (unison-code-rep co)))

; This adds a synchronization barrier around code loading. It uses
; a lock associated with the namespace, so this it will also be safe
; with regard to concurrent instantiations of any modules that get
; defined.
;
; It's possible that this could be made more fine grained. We were
; running into two issues in practice:
;
;   1. It was possible for a module to think it needs to declare
;      some combinators that actually occur in modules that are
;      depended upon, resulting in duplicate definiton errors.
;
;   2. It was possible for module-n to depend on module-m, but for
;      module-n to be defined an instantiated before module-m was
;      actually added to the namespace.
;
; This is due to how we keep track of which runtime definitions are
; in which module. There is a separate map storing those associations,
; and they are not inherently synchronized with the module registry.
; Any other synchronization scheme needs to account for these issues.
(define (add-runtime-code mname0 dfns0)
  (namespace-call-with-registry-lock runtime-namespace
    (lambda () (add-runtime-code-pre mname0 dfns0))))

(define (add-runtime-code-pre mname0 dfns0)
  ; flatten and filter out unnecessary definitions
  (define udefs
    (for/hash ([p (in-chunked-list dfns0)]
               #:when (need-code-loaded? (ufst p)))
      (splat-upair p)))

  (define-values (tmlinks codes)
    (for/lists (fsts snds)
               ([(fst snd) udefs])
      (values fst snd)))

  (cond
    ; short circuit if we have all the definitions loaded
    [(null? udefs) empty-chunked-list]
    [else
     (define deps (flatten (map code-dependencies codes)))
     ; classifying dependencies
     ;   hdeps - dependencies that are already loaded
     ;   ldeps - dependencies that we have code for, but need loading
     ;   ndeps - dependencies that we need code for
     ;   rdeps - ndeps that haven't been provided in dfns0
     (define-values (nldeps hdeps) (partition need-code-loaded? deps))
     (define-values (ndeps ldeps) (partition need-code? nldeps))
     (define rdeps (remove* tmlinks ndeps))
     (cond
       [(not (null? rdeps))
        (list->chunked-list rdeps)]

       [else
        ; add in definitions that haven't been loaded yet
        (define tdefs
          (hash-union udefs (resolve-unloaded ldeps)
            #:combine (lambda (_ y) y)))

        (add-runtime-code-proc mname0 tdefs)])]))

; Given a termlink and a list of dependencies for said link, tests
; if the code is recursive. This is done by seeing if it references
; any link with the same bytes. If it does, it must be (mututally)
; recursive. The only way for two definitions to get the same parent
; hash at this point is if they refer to one another.
(define (detect-recursion link deps)
  (define self (termlink-bytes link))
  (ormap (lambda (other)
           (match other
             [(unison-termlink-derived other _)
              (equal? self other)]
             [else #f]))
         deps))

(define (arity-tuple tl co)
  (unison-tuple
    (termlink->reference tl)
    (code-arity co)))

; Creates and adds a module for given module name and definitions.
;
; Passing #f for mname0 makes the procedure make up a fresh name.
;
; udefs should be a map associating termlinks to their code. It is
; assumed that udefs contains all the associations necessary to load
; the code successfully. So, any dependencies of the code in the map
; are either also in the map, or have already been loaded. The
; procedures that call into this one should have checked these already
; and given appropriate errors if we're missing code.
(define (add-runtime-code-proc mname0 udefs)
  ; Unpack the map into component lists
  (define-values (tmlinks codes arities depss)
    (for/lists (ls cs as ds)
               ([(tl co) udefs])
      (values
        tl
        co
        (arity-tuple tl co)
        (code-dependencies co))))

  (define tylinks (chunked-list->list (typelink-deps codes)))
  (define-values (ntylinks htylinks) (partition need-typelink? tylinks))

  (define hdeps (filter have-code-loaded? (flatten depss)))

  (define-values (lndefs lndecs dfns)
    (gen-codes:runtime (list->chunked-list arities) udefs))
  (define sdefs (append lndefs (append* dfns) lndecs))
  (define reqs (extra-requires htylinks hdeps))
  (define mname (or mname0 (generate-module-name tmlinks)))

  (expand-sandbox tmlinks depss)
  (register-code udefs)
  (add-module-type-associations
    (map reference->typelink ntylinks)
    mname)
  (add-module-term-associations tmlinks mname)
  (add-runtime-module mname reqs (list->chunked-list ntylinks) tmlinks sdefs)

  ; final result: no dependencies needed
  empty-chunked-list)

; Finds (transitively) code for references that we _know_ the code for,
; but which haven't been loaded into the runtime yet.
(define (resolve-unloaded need #:found [found (make-immutable-hash)])
  (match need
    ['() found]
    [(cons ln need)
     #:when (hash-has-key? found ln)
     (resolve-unloaded need #:found found)]
    [(cons ln need)
     (match (lookup-code ln)
       [(unison-sum 0 (list))
        (raise-argument-error
          'resolve-unloaded
          "have-code?"
          ln)]
       [(unison-sum 1 (list co))
        (define deps
          (filter need-code-loaded?
                  (code-dependencies co)))

        (resolve-unloaded
          (append need deps)
          #:found (hash-set found ln co))])]
    [else
      (raise-argument-error
        'resolve-unloaded
        "dependency list"
        need)]))

(define-unison-builtin (builtin-Code.cache_ dfns0)
  (add-runtime-code #f dfns0))

(define-unison-builtin (builtin-Value.load v0)
  (define val (unison-quote-val v0))
  (define deps
    (map reference->termlink
         (chunked-list->list (value-term-dependencies val))))

  (namespace-call-with-registry-lock runtime-namespace
    (lambda ()

      (define-values (ndeps hdeps) (partition need-code? deps))

      (cond
        [(not (null? ndeps))
         (ref-either-left (list->chunked-list ndeps))]
        [else
         (define ldeps (filter need-code-loaded? hdeps))
         (define to-load (resolve-unloaded ldeps))
         (add-runtime-code-proc #f to-load)
         (ref-either-right (reify-value val))]))))

(define-unison-builtin (builtin-Code.lookup tl)
  (match (lookup-code tl)
    [(unison-sum 0 (list)) ref-optional-none]
    [(unison-sum 1 (list co)) (ref-optional-some co)]))

(define-unison-builtin (builtin-validateSandboxed ok v)
  (let ([l (sandbox-scheme-value (chunked-list->list ok) v)])
    (null? l)))

(define-unison-builtin (builtin-sandboxLinks tl) (check-sandbox tl))

(define-unison-builtin (builtin-Code.isMissing tl)
  (cond
    [(unison-termlink-builtin? tl) #f]
    [(unison-termlink-con? tl) #f]
    [(have-code? tl) #t]
    [else #f]))

(define-unison-builtin (builtin-Value.validateSandboxed ok v)
  (sandbox-quoted (chunked-list->list ok) v))

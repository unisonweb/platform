#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define
         racket/exn
         racket/runtime-path
         (for-syntax racket/base)
         openssl/libcrypto
         unison/boot
         unison/chunked-seq
         racket/bool
        (only-in openssl/sha1 bytes->hex-string hex-string->bytes)

         )

(provide
  builtin-crypto.HashAlgorithm.Blake2b_256
  builtin-crypto.HashAlgorithm.Blake2b_256:termlink
  builtin-crypto.HashAlgorithm.Blake2b_512
  builtin-crypto.HashAlgorithm.Blake2b_512:termlink
  builtin-crypto.HashAlgorithm.Blake2s_256
  builtin-crypto.HashAlgorithm.Blake2s_256:termlink
  builtin-crypto.HashAlgorithm.Md5
  builtin-crypto.HashAlgorithm.Md5:termlink
  builtin-crypto.HashAlgorithm.Sha1
  builtin-crypto.HashAlgorithm.Sha1:termlink
  builtin-crypto.HashAlgorithm.Sha2_256
  builtin-crypto.HashAlgorithm.Sha2_256:termlink
  builtin-crypto.HashAlgorithm.Sha2_512
  builtin-crypto.HashAlgorithm.Sha2_512:termlink
  builtin-crypto.HashAlgorithm.Sha3_256
  builtin-crypto.HashAlgorithm.Sha3_256:termlink
  builtin-crypto.HashAlgorithm.Sha3_512
  builtin-crypto.HashAlgorithm.Sha3_512:termlink
  builtin-crypto.hashBytes
  builtin-crypto.hashBytes:termlink
  builtin-crypto.hmacBytes
  builtin-crypto.hmacBytes:termlink
  builtin-crypto.Ed25519.verify.impl
  builtin-crypto.Ed25519.verify.impl:termlink
  builtin-crypto.Ed25519.sign.impl
  builtin-crypto.Ed25519.sign.impl:termlink)

(define-runtime-path libb2-so '(so "libb2" ("1" #f)))

(define libb2
  (with-handlers [[exn:fail? exn->string]]
                 (ffi-lib libb2-so '("1" #f))))

(define _EVP-pointer (_cpointer 'EVP))

; returns a function that, when called, either
; 1) raises an exception, if libcrypto failed to load, or
; 2) returns a pair of (_EVP-pointer bits)
(define (lc-algo name bits)
  (if (string? libcrypto)
    (raise (error 'libcrypto "~a\n~a" name libcrypto))
    (let ([getter (get-ffi-obj name libcrypto (_fun -> _EVP-pointer))])
      (cons (getter) bits))))

(define (check v who)
  (unless (= 1 v)
    (error who "failed with return value ~a" v)))

(define EVP_Digest
    (if (string? libcrypto)
        (lambda _ (raise (error 'libcrypto "EVP_Digest\n~a" libcrypto)))
        (get-ffi-obj "EVP_Digest" libcrypto
            (_fun
                _pointer ; input
                _int     ; input-len
                _pointer ; output
                _pointer ; null
                _EVP-pointer ; algorithm
                _pointer ; null
            -> (r : _int)
            -> (unless (= 1 r)
                    (error 'EVP_Digest "failed with return value ~a" r))))))

(define HMAC
    (if (string? libcrypto)
        (lambda _ (raise (error 'libcrypto "HMAC\n~a" libcrypto)))
        (get-ffi-obj "HMAC" libcrypto
            (_fun
                _EVP-pointer ; algorithm
                _pointer     ; key
                _int         ; key-len
                _pointer     ; input
                _int         ; input-len
                _pointer     ; output pointer
                _pointer     ; null
            -> _pointer ; unused
            ))))

(define (libb2-raw fn)
    (if (string? libb2)
        (lambda _ (raise (error 'libb2 "~a\n~a" fn libb2)))
        (get-ffi-obj fn libb2
            (_fun
                _pointer ; output
                _pointer ; input
                _pointer ; key
                _int     ; output-len
                _int     ; input-len
                _int     ; key-len
            -> (r : _int)
            -> (unless (= 0 r)
                    (error 'blake2 "~a failed with return value ~a" fn r))))))

(define blake2b-raw (libb2-raw "blake2b"))
(define blake2s-raw (libb2-raw "blake2s"))

(define-unison-builtin #:hints [value]
  (builtin-crypto.HashAlgorithm.Md5)
  (lc-algo "EVP_md5" 128))

(define-unison-builtin #:hints [value]
  (builtin-crypto.HashAlgorithm.Sha1)
  (lc-algo "EVP_sha1" 160))

(define-unison-builtin #:hints [value]
  (builtin-crypto.HashAlgorithm.Sha2_256)
  (lc-algo "EVP_sha256" 256))

(define-unison-builtin #:hints [value]
  (builtin-crypto.HashAlgorithm.Sha2_512)
  (lc-algo "EVP_sha512" 512))

(define-unison-builtin #:hints [value]
  (builtin-crypto.HashAlgorithm.Sha3_256)
  (lc-algo "EVP_sha3_256" 256))

(define-unison-builtin #:hints [value]
  (builtin-crypto.HashAlgorithm.Sha3_512)
  (lc-algo "EVP_sha3_512" 512))

(define _EVP_PKEY-pointer (_cpointer 'EVP_PKEY))
(define _EVP_MD_CTX-pointer (_cpointer 'EVP_MD_CTX))

(define EVP_MD_CTX_new
    (if (string? libcrypto)
        (lambda _ (raise (error 'libcrypto "EVP_MD_CTX_create\n~a" libcrypto)))
        (get-ffi-obj "EVP_MD_CTX_new" libcrypto
            (_fun -> _EVP_MD_CTX-pointer
            ))))

; EVP_PKEY_new_raw_private_key(int type, NULL, const unsigned char *key, size_t keylen);
(define EVP_PKEY_new_raw_private_key
    (if (string? libcrypto)
        (lambda _ (raise (error 'libcrypto "EVP_PKEY_new_raw_private_key\n~a" libcrypto)))
        (get-ffi-obj "EVP_PKEY_new_raw_private_key" libcrypto
            (_fun
                _int       ; type
                _pointer   ; engine (null)
                _pointer   ; key
                _int       ; key-len
            -> _EVP_PKEY-pointer
            ))))

; EVP_DigestSignInit(hctx, NULL, EVP_sha256(), NULL, pkey)
(define EVP_DigestSignInit
    (if (string? libcrypto)
        (lambda _ (raise (error 'libcrypto "EVP_DigestSignInit\n~a" libcrypto)))
        (get-ffi-obj "EVP_DigestSignInit" libcrypto
            (_fun
                _EVP_MD_CTX-pointer
                _pointer          ; (null)
                _pointer          ; (null)
                _pointer          ; (null)
                _EVP_PKEY-pointer ; pkey
            -> _int
            ))))

; EVP_DigestSign(hctx, output, output-len-ptr, input-data, input-data-len)
(define EVP_DigestSign
    (if (string? libcrypto)
        (lambda _ (raise (error 'libcrypto "EVP_DigestSign\n~a" libcrypto)))
        (get-ffi-obj "EVP_DigestSign" libcrypto
            (_fun
                _EVP_MD_CTX-pointer
                _pointer  ; output
                (_ptr o _int)  ; output-len (null prolly)
                _pointer  ; input-data
                _int      ; input-data-len
            -> _int
            ))))

; EVP_PKEY_new_raw_public_key(int type, NULL, const unsigned char *key, size_t keylen);
(define EVP_PKEY_new_raw_public_key
    (if (string? libcrypto)
        (lambda _ (raise (error 'libcrypto "EVP_PKEY_new_raw_public_key\n~a" libcrypto)))
        (get-ffi-obj "EVP_PKEY_new_raw_public_key" libcrypto
            (_fun
                _int       ; type
                _pointer   ; engine (null)
                _pointer   ; key
                _int       ; key-len
            -> _EVP_PKEY-pointer
            ))))

; int EVP_DigestVerifyInit(EVP_MD_CTX *ctx, EVP_PKEY_CTX **pctx,
;                          const EVP_MD *type, ENGINE *e, EVP_PKEY *pkey);
(define EVP_DigestVerifyInit
    (if (string? libcrypto)
        (lambda _ (raise (error 'libcrypto "EVP_DigestVerifyInit\n~a" libcrypto)))
        (get-ffi-obj "EVP_DigestVerifyInit" libcrypto
            (_fun
                _EVP_MD_CTX-pointer
                _pointer          ; (null)
                _pointer          ; (null)
                _pointer          ; (null)
                _EVP_PKEY-pointer ; pkey
            -> _int
            ))))

; int EVP_DigestVerify(EVP_MD_CTX *ctx, const unsigned char *sig,
;                      size_t siglen, const unsigned char *tbs, size_t tbslen);
(define EVP_DigestVerify
    (if (string? libcrypto)
        (lambda _ (raise (error 'libcrypto "EVP_DigestVerify\n~a" libcrypto)))
        (get-ffi-obj "EVP_DigestVerify" libcrypto
            (_fun
                _EVP_MD_CTX-pointer
                _pointer  ; signature
                _int      ; signature-len
                _pointer  ; input-data
                _int      ; input-data-len
            -> _int
            ))))


(define EVP_PKEY_ED25519 1087)
(define (evpSign-raw seed input)
  (let* ([ctx (EVP_MD_CTX_new)]
         [pkey (EVP_PKEY_new_raw_private_key EVP_PKEY_ED25519 #f seed (bytes-length seed))])
    (if (false? pkey)
      (raise (error "Invalid seed provided."))
      (if (<= (EVP_DigestSignInit ctx #f #f #f pkey) 0)
        (raise (error "Initializing signing failed"))
        (let* ([output (make-bytes 64)])
          (if (<= (EVP_DigestSign ctx output input (bytes-length input)) 0)
            (raise (error "Running digest failed"))
            output))))))

(define (evpVerify-raw public-key input signature)
  (let* ([ctx (EVP_MD_CTX_new)]
         [pkey (EVP_PKEY_new_raw_public_key EVP_PKEY_ED25519 #f public-key (bytes-length public-key))])
    (if (false? pkey)
      (raise (error "Invalid seed provided."))
      (if (<= (EVP_DigestVerifyInit ctx #f #f #f pkey) 0)
        (raise (error "Initializing Verify failed"))
        (if (<= (EVP_DigestVerify ctx signature (bytes-length signature) input (bytes-length input)) 0)
          #f
          #t)))))

(define-unison-builtin
  (builtin-crypto.Ed25519.sign.impl seed _ignored_pubkey input)
  (bytes->chunked-bytes
    (evpSign-raw
      (chunked-bytes->bytes seed)
      (chunked-bytes->bytes input))))

(define-unison-builtin
  (builtin-crypto.Ed25519.verify.impl public-key input signature)
  (evpVerify-raw
    (chunked-bytes->bytes public-key)
    (chunked-bytes->bytes input)
    (chunked-bytes->bytes signature)))

(define-unison-builtin #:hints [value]
  (builtin-crypto.HashAlgorithm.Blake2s_256)
  (cons 'blake2s 256))
(define-unison-builtin #:hints [value]
  (builtin-crypto.HashAlgorithm.Blake2b_512)
  (cons 'blake2b 512))

; This one isn't provided by libcrypto, for some reason
(define-unison-builtin #:hints [value]
  (builtin-crypto.HashAlgorithm.Blake2b_256)
  (cons 'blake2b 256))

; kind is a pair of (algorithm bits)
; where algorithm is either an EVP_pointer for libcrypto functions,
; or the tag 'blake2b for libb2 function.
(define-unison-builtin (builtin-crypto.hashBytes kind input)
  (bytes->chunked-bytes
    (hashBytes-raw kind (chunked-bytes->bytes input))))

; kind is a pair of (algorithm bits)
; where algorithm is either an EVP_pointer for libcrypto functions,
; or the tag 'blake2b for libb2 function.
(define (hashBytes-raw kind input)
  (let* ([bytes (/ (cdr kind) 8)]
         [output (make-bytes bytes)]
         [algo (car kind)])
    (case algo
      ['blake2b (blake2b-raw output input #f bytes (bytes-length input) 0)]
      ['blake2s (blake2s-raw output input #f bytes (bytes-length input) 0)]
      [else (EVP_Digest input (bytes-length input) output #f algo #f)])

    output))

; Mutates and returns the first argument
(define (xor one two)
  (for ([i (in-range (bytes-length one))])
    (bytes-set! one i
                (bitwise-xor
                 (bytes-ref one i)
                 (bytes-ref two i))))
  one)

; doing the blake hmac by hand. libcrypto
; supports hmac natively, so we just defer to that
(define (hmacBlake kind key input)
  (let*
      ([bytes (/ (cdr kind) 8)]
       [blocksize (case (car kind) ['blake2b 128] ['blake2s 64])]

       [key_
        (let ([key_ (make-bytes blocksize 0)])
          (bytes-copy! key_ 0
                       (if (< blocksize (bytes-length key))
                           (hashBytes-raw kind key)
                           key))
          key_)]

       [opad (xor (make-bytes blocksize #x5c) key_)]
       [ipad (xor (make-bytes blocksize #x36) key_)]

       [full (bytes-append
              opad
              (hashBytes-raw kind (bytes-append ipad input)))])
    (hashBytes-raw kind full)))

(define-unison-builtin (builtin-crypto.hmacBytes kind key input)
  (bytes->chunked-bytes
    (hmacBytes-raw
      kind
      (chunked-bytes->bytes key)
      (chunked-bytes->bytes input))))

(define (hmacBytes-raw kind key input)
  (case (car kind)
    ['blake2b (hmacBlake kind key input)]
    ['blake2s (hmacBlake kind key input)]
    [else
     (let* ([bytes (/ (cdr kind) 8)]
            [output (make-bytes bytes)]
            [algo (car kind)])
       (HMAC algo key (bytes-length key) input (bytes-length input) output #f)
       output)]))


; These will only be evaluated by `raco test`
(module+ test
  (require rackunit
           (only-in openssl/sha1 bytes->hex-string hex-string->bytes))

  (test-case "ed25519 sign"
    (check-equal?
        (bytes->hex-string
          (evpSign-raw
            (hex-string->bytes "0000000000000000000000000000000000000000000000000000000000000000") #""))
        "8f895b3cafe2c9506039d0e2a66382568004674fe8d237785092e40d6aaf483e4fc60168705f31f101596138ce21aa357c0d32a064f423dc3ee4aa3abf53f803"))

  (test-case "ed25519 verify"
    (check-equal?
        (evpVerify-raw
          (hex-string->bytes "3b6a27bcceb6a42d62a3a8d02a6f0d73653215771de243a63ac048a18b59da29")
          #""
          (hex-string->bytes "8f895b3cafe2c9506039d0e2a66382568004674fe8d237785092e40d6aaf483e4fc60168705f31f101596138ce21aa357c0d32a064f423dc3ee4aa3abf53f803")
          )
        #t))

  (test-case "sha1 hmac"
    (check-equal?
     (bytes->hex-string (hmacBytes-raw (builtin-crypto.HashAlgorithm.Sha1) #"key" #"message"))
     "2088df74d5f2146b48146caf4965377e9d0be3a4"))

  (test-case "blake2b-256 hmac"
    (check-equal?
     (bytes->hex-string (hmacBytes-raw (builtin-crypto.HashAlgorithm.Blake2b_256) #"key" #"message"))
     "442d98a3872d3f56220f89e2b23d0645610b37c33dd3315ef224d0e39ada6751"))

  (test-case "blake2b-512 hmac"
    (check-equal?
     (bytes->hex-string (hmacBytes-raw (builtin-crypto.HashAlgorithm.Blake2b_512) #"key" #"message"))
     "04e9ada930688cde75eec939782eed653073dd621d7643f813702976257cf037d325b50eedd417c01b6ad1f978fbe2980a93d27d854044e8626df6fa279d6680"))

  (test-case "blake2s-256 hmac"
    (check-equal?
     (bytes->hex-string (hmacBytes-raw (builtin-crypto.HashAlgorithm.Blake2s_256) #"key" #"message"))
     "bba8fa28708ae80d249e317318c95c859f3f77512be23910d5094d9110454d6f"))

  (test-case "md5 basic"
    (check-equal?
     (bytes->hex-string (hashBytes-raw (builtin-crypto.HashAlgorithm.Md5) #""))
     "d41d8cd98f00b204e9800998ecf8427e"))

  (test-case "sha1 basic"
    (check-equal?
     (bytes->hex-string (hashBytes-raw (builtin-crypto.HashAlgorithm.Sha1) #""))
     "da39a3ee5e6b4b0d3255bfef95601890afd80709"))

  (test-case "sha2-256 basic"
    (check-equal?
     (bytes->hex-string (hashBytes-raw (builtin-crypto.HashAlgorithm.Sha2_256) #""))
     "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))

  (test-case "sha2-512 basic"
    (check-equal?
     (bytes->hex-string (hashBytes-raw (builtin-crypto.HashAlgorithm.Sha2_512) #""))
     "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e"))

  (test-case "sha3-256 basic"
    (check-equal?
     (bytes->hex-string (hashBytes-raw (builtin-crypto.HashAlgorithm.Sha3_256) #""))
     "a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a"))

  (test-case "sha3-512 basic"
    (check-equal?
     (bytes->hex-string (hashBytes-raw (builtin-crypto.HashAlgorithm.Sha3_512) #""))
     "a69f73cca23a9ac5c8b567dc185a756e97c982164fe25859e0d1dcc1475c80a615b2123af1f5f94c11e3e9402c3ac558f500199d95b6d3e301758586281dcd26"))

  (test-case "blake2s_256 basic"
    (check-equal?
     (bytes->hex-string (hashBytes-raw (builtin-crypto.HashAlgorithm.Blake2s_256) #""))
     "69217a3079908094e11121d042354a7c1f55b6482ca1a51e1b250dfd1ed0eef9"))

  (test-case "blake2b_256 basic"
    (check-equal?
     (bytes->hex-string (hashBytes-raw (builtin-crypto.HashAlgorithm.Blake2b_256) #""))
     "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"))

  (test-case "blake2b_512 basic"
    (check-equal?
     (bytes->hex-string (hashBytes-raw (builtin-crypto.HashAlgorithm.Blake2b_512) #""))
     "786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce")))

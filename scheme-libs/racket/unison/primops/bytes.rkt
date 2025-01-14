
#lang racket/base

(require unison/boot
         unison/bytes-nat
         unison/chunked-bytes
         unison/chunked-seq
         unison/data
         unison/data-info
         unison/gzip
         unison/string-search
         unison/zlib)

(provide
  builtin-Bytes.++
  builtin-Bytes.++:termlink
  builtin-Bytes.at
  builtin-Bytes.at:termlink
  builtin-Bytes.decodeNat16be
  builtin-Bytes.decodeNat16be:termlink
  builtin-Bytes.decodeNat16le
  builtin-Bytes.decodeNat16le:termlink
  builtin-Bytes.decodeNat32be
  builtin-Bytes.decodeNat32be:termlink
  builtin-Bytes.decodeNat32le
  builtin-Bytes.decodeNat32le:termlink
  builtin-Bytes.decodeNat64be
  builtin-Bytes.decodeNat64be:termlink
  builtin-Bytes.decodeNat64le
  builtin-Bytes.decodeNat64le:termlink
  builtin-Bytes.drop
  builtin-Bytes.drop:termlink
  builtin-Bytes.empty
  builtin-Bytes.empty:termlink
  builtin-Bytes.encodeNat16be
  builtin-Bytes.encodeNat16be:termlink
  builtin-Bytes.encodeNat16le
  builtin-Bytes.encodeNat16le:termlink
  builtin-Bytes.encodeNat32be
  builtin-Bytes.encodeNat32be:termlink
  builtin-Bytes.encodeNat32le
  builtin-Bytes.encodeNat32le:termlink
  builtin-Bytes.encodeNat64be
  builtin-Bytes.encodeNat64be:termlink
  builtin-Bytes.encodeNat64le
  builtin-Bytes.encodeNat64le:termlink
  builtin-Bytes.flatten
  builtin-Bytes.flatten:termlink
  builtin-Bytes.fromBase16
  builtin-Bytes.fromBase16:termlink
  builtin-Bytes.fromBase32
  builtin-Bytes.fromBase32:termlink
  builtin-Bytes.fromBase64
  builtin-Bytes.fromBase64:termlink
  builtin-Bytes.fromBase64UrlUnpadded
  builtin-Bytes.fromBase64UrlUnpadded:termlink
  builtin-Bytes.fromList
  builtin-Bytes.fromList:termlink
  builtin-Bytes.gzip.compress
  builtin-Bytes.gzip.compress:termlink
  builtin-Bytes.gzip.decompress
  builtin-Bytes.gzip.decompress:termlink
  builtin-Bytes.indexOf
  builtin-Bytes.indexOf:termlink
  builtin-Bytes.size
  builtin-Bytes.size:termlink
  builtin-Bytes.take
  builtin-Bytes.take:termlink
  builtin-Bytes.toBase16
  builtin-Bytes.toBase16:termlink
  builtin-Bytes.toBase32
  builtin-Bytes.toBase32:termlink
  builtin-Bytes.toBase64
  builtin-Bytes.toBase64:termlink
  builtin-Bytes.toBase64UrlUnpadded
  builtin-Bytes.toBase64UrlUnpadded:termlink
  builtin-Bytes.toList
  builtin-Bytes.toList:termlink
  builtin-Bytes.zlib.compress
  builtin-Bytes.zlib.compress:termlink
  builtin-Bytes.zlib.decompress
  builtin-Bytes.zlib.decompress:termlink)

(define-unison-builtin (builtin-Bytes.++ l r)
  (chunked-bytes-append l r))

(define-unison-builtin (builtin-Bytes.at n bs)
  (with-handlers
    ([exn:fail:contract? (lambda (e) ref-optional-none)])
    (ref-optional-some (chunked-bytes-ref bs n))))

(define-unison-builtin (builtin-Bytes.decodeNat16be bs)
  (decodeNatBe bs 2))

(define-unison-builtin (builtin-Bytes.decodeNat16le bs)
  (decodeNatLe bs 2))

(define-unison-builtin (builtin-Bytes.decodeNat32be bs)
  (decodeNatBe bs 4))

(define-unison-builtin (builtin-Bytes.decodeNat32le bs)
  (decodeNatLe bs 4))

(define-unison-builtin (builtin-Bytes.decodeNat64be bs)
  (decodeNatBe bs 8))

(define-unison-builtin (builtin-Bytes.decodeNat64le bs)
  (decodeNatLe bs 8))

(define-unison-builtin (builtin-Bytes.drop n bs)
  (chunked-bytes-drop bs n))

(define-unison-builtin #:hints [value] (builtin-Bytes.empty)
  empty-chunked-bytes)

(define-unison-builtin (builtin-Bytes.encodeNat16be n)
  (encodeNatBe n 2))

(define-unison-builtin (builtin-Bytes.encodeNat16le n)
  (encodeNatLe n 2))

(define-unison-builtin (builtin-Bytes.encodeNat32be n)
  (encodeNatBe n 4))

(define-unison-builtin (builtin-Bytes.encodeNat32le n)
  (encodeNatLe n 4))

(define-unison-builtin (builtin-Bytes.encodeNat64be n)
  (encodeNatBe n 8))

(define-unison-builtin (builtin-Bytes.encodeNat64le n)
  (encodeNatLe n 8))

; Note: the current implementation has no mechanism for
; flattening the representation, but in the event this changes,
; this should be revisited.
(define-unison-builtin (builtin-Bytes.flatten bs) bs)

(define-unison-builtin (builtin-Bytes.fromBase16 bs)
  (with-handlers
    ([exn:fail? (lambda (e) (ref-either-left (exception->string e)))])
    (ref-either-right (base16-decode bs))))

(define-unison-builtin (builtin-Bytes.fromBase32 bs)
  (with-handlers
    ([exn:fail? (lambda (e) (ref-either-left (exception->string e)))])
    (ref-either-right (base32-decode bs))))

(define-unison-builtin (builtin-Bytes.fromBase64 bs)
  (with-handlers
    ([exn:fail? (lambda (e) (ref-either-left (exception->string e)))])
    (ref-either-right (base64-decode bs))))

(define-unison-builtin (builtin-Bytes.fromBase64UrlUnpadded bs)
  (with-handlers
    ([exn:fail? (lambda (e) (ref-either-left (exception->string e)))])
    (ref-either-right (base64-decode bs #:padded? #f))))

(define-unison-builtin (builtin-Bytes.fromList l)
  (build-chunked-bytes
    (chunked-list-length l)
    (lambda (i) (chunked-list-ref l i))))

(define-unison-builtin (builtin-Bytes.gzip.compress bs)
  (bytes->chunked-bytes (gzip-bytes (chunked-bytes->bytes bs))))

(define-unison-builtin (builtin-Bytes.gzip.decompress bs)
  (with-handlers
    [[exn:fail? (lambda (e) (ref-either-left (exception->string e)))]]
    (ref-either-right
      (bytes->chunked-bytes
        (gunzip-bytes
          (chunked-bytes->bytes bs))))))

(define-unison-builtin (builtin-Bytes.size bs)
  (chunked-bytes-length bs))

(define-unison-builtin (builtin-Bytes.take n bs)
  (chunked-bytes-take bs n))

(define-unison-builtin (builtin-Bytes.toBase16 bs)
  (base16-encode bs))

(define-unison-builtin (builtin-Bytes.toBase32 bs)
  (base32-encode bs))

(define-unison-builtin (builtin-Bytes.toBase64 bs)
  (base64-encode bs))

(define-unison-builtin (builtin-Bytes.toBase64UrlUnpadded bs)
  (base64-encode bs #:pad? #f))

(define-unison-builtin (builtin-Bytes.toList bs)
  (build-chunked-list
    (chunked-bytes-length bs)
    (lambda (i) (chunked-bytes-ref bs i))))

(define-unison-builtin (builtin-Bytes.zlib.compress bs)
  (bytes->chunked-bytes
    (zlib-deflate-bytes
      (chunked-bytes->bytes bs))))

(define-unison-builtin (builtin-Bytes.zlib.decompress bs)
  (with-handlers
    [[exn:fail?
       (lambda (e)
         (ref-either-left
           (ref-failure-failure
             ref-miscfailure:typelink
             (exception->string e)
             (unison-any-any ref-unit-unit))))]]
    (ref-either-right
      (bytes->chunked-bytes
        (zlib-inflate-bytes
          (chunked-bytes->bytes bs))))))

(define-unison-builtin (builtin-Bytes.indexOf n h)
  (define v (chunked-bytes-index-of h n))

  (if v
    (ref-optional-some v)
    ref-optional-none))

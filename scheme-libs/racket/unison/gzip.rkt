#lang racket/base
(require unison/data
         file/gzip
         file/gunzip
         unison/core
         (only-in unison/chunked-seq
            bytes->chunked-bytes
            chunked-bytes->bytes))

(provide
  gzip-bytes
  gunzip-bytes)

(define (gzip-bytes bytes)
    (let ([op1 (open-output-bytes)])
        (gzip-through-ports (open-input-bytes bytes) op1 #f 0)
        (get-output-bytes op1)))

(define (gunzip-bytes bytes)
    (let ([op1 (open-output-bytes)])
        (gunzip-through-ports (open-input-bytes bytes) op1)
        (get-output-bytes op1)))

(define (gzip.compress bytes)
    (bytes->chunked-bytes (gzip-bytes (chunked-bytes->bytes bytes))))

(define (gzip.decompress bytes)
  (with-handlers
    [[exn:fail? (lambda (e) (left (exception->string e)))]]
    (right
      (bytes->chunked-bytes
        (gunzip-bytes
          (chunked-bytes->bytes bytes))))))

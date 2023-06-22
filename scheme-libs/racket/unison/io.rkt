#lang racket/base
(require unison/data
         unison/chunked-seq
         unison/core
         racket/flonum
         (only-in
           rnrs/arithmetic/flonums-6
           flmod))

(provide
 (prefix-out
  unison-FOp-Clock.internals.
  (combine-out
    threadCPUTime.v1
    processCPUTime.v1
    realtime.v1
    monotonic.v1
    sec.v1
    nsec.v1))
 (prefix-out
  unison-FOp-IO.
  (combine-out
   fileExists.impl.v3
   getFileTimestamp.impl.v3
   getTempDirectory.impl.v3
   removeFile.impl.v3
   getFileSize.impl.v3)))

(define (getFileSize.impl.v3 path)
    (with-handlers
        [[exn:fail:filesystem? (lambda (e) (exception "IOFailure" (exception->string e) '()))]]
        (right (file-size (chunked-string->string path)))))

(define (getFileTimestamp.impl.v3 path)
    (with-handlers
        [[exn:fail:filesystem? (lambda (e) (exception "IOFailure" (exception->string e) '()))]]
        (right (file-or-directory-modify-seconds (chunked-string->string path)))))

(define (fileExists.impl.v3 path)
    (right (bool (file-exists? (chunked-string->string path)))))

(define (removeFile.impl.v3 path)
    (delete-file (chunked-string->string path))
    (right none))

(define (getTempDirectory.impl.v3)
    (right (string->chunked-string (path->string (find-system-path 'temp-dir)))))

(define (threadCPUTime.v1)
    (right (current-process-milliseconds (current-thread))))
(define (processCPUTime.v1)
    (right (current-process-milliseconds 'process)))
(define (realtime.v1)
    (right (current-inexact-milliseconds)))
(define (monotonic.v1)
    (right (current-inexact-monotonic-milliseconds)))

; 
(define (flt f) (fl->exact-integer (fltruncate f)))

(define (sec.v1 ts) (flt (/ ts 1000)))

(define (nsec.v1 ts) (flt (* (flmod ts 1000.0) 1000000)))

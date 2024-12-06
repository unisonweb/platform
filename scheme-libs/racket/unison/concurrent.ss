#!r6rs

(library (unison concurrent)
  (export
    ref-new
    ref-read
    ref-write
    ref-cas
    promise-new
    promise-read
    promise-write
    promise-try-read
    fork
    kill
    sleep)

  (import (rnrs)
          (rnrs records syntactic)
          (unison data)
          (unison data-info)
          (unison core)
          (unison chunked-seq)
          (rename
           (only (racket base)
                 box
                 car
                 unbox
                 set-box!
                 box-cas!
                 make-semaphore
                 semaphore-peek-evt
                 semaphore-post
                 sync/enable-break
                 thread
                 break-thread
                 parameterize-break
                 sleep
                 printf
                 with-handlers
                 exn:break?)
           (box ref-new)
           (car icar)
           (unbox ref-read)
           (set-box! ref-write)
           (sleep sleep-secs))
          (only (racket unsafe ops) unsafe-struct*-cas!))

  (define-record-type promise (fields semaphore event (mutable value)))

  (define (promise-new)
    (let* ([sem (make-semaphore)]
           [evt (semaphore-peek-evt sem)]
           [value ref-optional-none])
      (make-promise sem evt value)))

  (define (promise-try-read promise) (promise-value promise))

  (define (promise-read promise)
    (let loop ()
      (let ([value (promise-value promise)])
        (cond
          [(= (unison-data-tag value) ref-optional-some:tag)
           (icar (unison-data-fields value))]
          [else (sync/enable-break (promise-event promise)) (loop)]))))

  (define (promise-write promise new-value)
    (let loop ()
      (let* ([value (promise-value promise)]
             [cas! (lambda ()
                     (unsafe-struct*-cas!
                       promise 2
                       value
                       (ref-optional-some new-value)))]
             [awake-readers (lambda ()
                              (semaphore-post
                                (promise-semaphore promise)))])
        (cond
          [(= (unison-data-tag value) ref-optional-some:tag) #f]
          [else
           (let ([ok (parameterize-break #f (if (cas!) (awake-readers) #f))])
             (if ok #t (loop)))]))))

  (define (ref-cas ref ticket value)
    (if (box-cas! ref ticket value) #t #f))

  (define (sleep n)
    (sleep-secs (/ n 1000000))
    (ref-either-right ref-unit-unit))

  ;; Swallows uncaught breaks/thread kills rather than logging them to
  ;; match the behaviour of the Haskell runtime
  (define (fork thunk)
    (thread
     (lambda ()
       (with-handlers ([exn:break? (lambda (x) ())])
         (thunk)))))

  (define (kill threadId)
    (break-thread threadId)
    (ref-either-right ref-unit-unit))
  )

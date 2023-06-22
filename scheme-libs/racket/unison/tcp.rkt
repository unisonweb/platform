; TCP primitives!
#lang racket/base
(require racket/exn
         racket/match
         racket/tcp
         unison/data
         unison/chunked-seq
         unison/core)

(provide
 socket-pair-input
 socket-pair-output
 (prefix-out
  unison-FOp-IO.
  (combine-out
   clientSocket.impl.v3
   closeSocket.impl.v3
   socketReceive.impl.v3
   socketPort.impl.v3
   serverSocket.impl.v3
   listen.impl.v3
   socketAccept.impl.v3
   socketSend.impl.v3)))

(struct socket-pair (input output))

(define (handle-errors fn)
  (with-handlers
      [[exn:fail:network? (lambda (e) (exception "IOFailure" (exception->string e) '()))]
       [exn:fail:contract? (lambda (e) (exception "InvalidArguments" (exception->string e) '()))]
       [(lambda _ #t) (lambda (e) (exception "MiscFailure" (chunked-string->string (format "Unknown exception ~a" (exn->string e))) e))] ]
    (fn)))

(define (closeSocket.impl.v3 socket)
  (handle-errors
   (lambda ()
     (if (socket-pair? socket)
         (begin
           (close-input-port (socket-pair-input socket))
           (close-output-port (socket-pair-output socket)))
         (tcp-close socket))
     (right none))))

(define (clientSocket.impl.v3 host port) ; string string -> socket-pair
  (handle-errors
   (lambda ()
     (let-values ([(input output) (tcp-connect (chunked-string->string host) (string->number (chunked-string->string port)))])
       (right (socket-pair input output))))))

(define (socketSend.impl.v3 socket data) ; socket bytes -> ()
  (if (not (socket-pair? socket))
      (exception "InvalidArguments" "Cannot send on a server socket" '())
      (begin
        (write-bytes (chunked-bytes->bytes data) (socket-pair-output socket))
        (flush-output (socket-pair-output socket))
        (right none)))); )

(define (socketReceive.impl.v3 socket amt) ; socket int -> bytes
  (if (not (socket-pair? socket))
      (exception "InvalidArguments" "Cannot receive on a server socket")
      (handle-errors
       (lambda ()
         (begin
           (let* ([buffer (make-bytes amt)]
                  [read (read-bytes-avail! buffer (socket-pair-input socket))])
             (right (bytes->chunked-bytes (subbytes buffer 0 read)))))))))

(define (socketPort.impl.v3 socket)
  (let-values ([(_ local-port __ ___) (tcp-addresses
                                       (if (socket-pair? socket)
                                           (socket-pair-input socket)
                                           socket) #t)])
    (right local-port)))

(define serverSocket.impl.v3 ; string -> socket (or) string string -> socket
  (lambda args
    (let-values ([(hostname port)
                  (match args
                    [(list _ port) (values #f (chunked-string->string port))]
                    [(list _ hostname port) (values
                                             (chunked-string->string hostname)
                                             (chunked-string->string port))])])

      (with-handlers
          [[exn:fail:network? (lambda (e) (exception "IOFailure" (exception->string e) '()))]
           [exn:fail:contract? (lambda (e) (exception "InvalidArguments" (exception->string e) '()))]
           [(lambda _ #t) (lambda (e) (exception "MiscFailure" (string->chunked-string "Unknown exception") e))] ]
        (let ([listener (tcp-listen (string->number port ) 4 #f (if (equal? 0 hostname) #f hostname))])
          (right listener))))))

; NOTE: This is a no-op because racket's public TCP stack doesn't have separate operations for
; "bind" vs "listen". We've decided to have `serverSocket` do the "bind & listen", and have
; this do nothing.
; If we want ~a little better parity with the haskell implementation, we might set a flag or
; something on the listener, and error if you try to `accept` on a server socket that you haven't
; called `listen` on yet.
(define (listen.impl.v3 _listener)
  (right none))

(define (socketAccept.impl.v3 listener)
  (if (socket-pair? listener)
      (exception "InvalidArguments" (string->chunked-string "Cannot accept on a non-server socket"))
      (begin
        (let-values ([(input output) (tcp-accept listener)])
          (right (socket-pair input output))))))

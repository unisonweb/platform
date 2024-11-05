; TCP primitives!
#lang racket/base
(require racket/exn
         racket/match
         racket/tcp
         unison/boot
         unison/data
         unison/data-info
         unison/chunked-seq
         unison/network-utils)

(provide
  builtin-IO.clientSocket.impl.v3
  builtin-IO.clientSocket.impl.v3:termlink
  builtin-IO.closeSocket.impl.v3
  builtin-IO.closeSocket.impl.v3:termlink
  builtin-IO.listen.impl.v3
  builtin-IO.listen.impl.v3:termlink
  builtin-IO.serverSocket.impl.v3
  builtin-IO.serverSocket.impl.v3:termlink
  builtin-IO.socketAccept.impl.v3
  builtin-IO.socketAccept.impl.v3:termlink
  builtin-IO.socketPort.impl.v3
  builtin-IO.socketPort.impl.v3:termlink
  builtin-IO.socketReceive.impl.v3
  builtin-IO.socketReceive.impl.v3:termlink
  builtin-IO.socketSend.impl.v3
  builtin-IO.socketSend.impl.v3:termlink
  builtin-Socket.toText
  builtin-Socket.toText:termlink)

(define-unison-builtin (builtin-IO.closeSocket.impl.v3 socket)
  (handle-errors
    (if (socket-pair? socket)
      (begin
        (close-input-port (socket-pair-input socket))
        (close-output-port (socket-pair-output socket)))
      (tcp-close socket))
    (ref-either-right ref-unit-unit)))

; string string -> either failure socket-pair
(define-unison-builtin (builtin-IO.clientSocket.impl.v3 host port)
  (handle-errors
    (let-values
      ([(input output) (tcp-connect
                         (chunked-string->string host)
                         (string->number
                           (chunked-string->string port)))])
      (ref-either-right (socket-pair input output)))))

; socket bytes -> either failure ()
(define-unison-builtin (builtin-IO.socketSend.impl.v3 socket data)
  (if (not (socket-pair? socket))
      (ref-either-left
        (ref-failure-failure
          ref-iofailure:typelink
          (string->chunked-string "Cannot send on a server socket")
          (unison-any-any ref-unit-unit)))
      (begin
        (write-bytes (chunked-bytes->bytes data) (socket-pair-output socket))
        (flush-output (socket-pair-output socket))
        (ref-either-right ref-unit-unit))))

; socket int -> either failure bytes
(define-unison-builtin (builtin-IO.socketReceive.impl.v3 socket amt)
  (if (not (socket-pair? socket))
      (ref-either-left
        (ref-failure-failure
          ref-iofailure:typelink
          (string->chunked-string "Cannot receive on a server socket")
          (unison-any-any ref-unit-unit)))

      (handle-errors
        (define buffer (make-bytes amt))
        (define read
          (read-bytes-avail! buffer (socket-pair-input socket)))

        (ref-either-right
          (bytes->chunked-bytes (subbytes buffer 0 read))))))

; socket -> either failure nat
(define-unison-builtin (builtin-IO.socketPort.impl.v3 socket)
  (define-values (_ local-port __ ___)
    (tcp-addresses
      (if (socket-pair? socket)
        (socket-pair-input socket)
        socket)
      #t))

  (ref-either-right local-port))

(define (left-fail-exn e)
  (ref-either-left
    (ref-failure-failure
      ref-iofailure:typelink
      (exception->string e)
      (unison-any-any ref-unit-unit))))

(define (left-fail-k e)
  (ref-either-left
    (ref-failure-failure
      ref-miscfailure:typelink
      (string->chunked-string "Unknown exception")
      (unison-any-any ref-unit-unit))))

; optional string -> string -> either failure socket
(define-unison-builtin (builtin-IO.serverSocket.impl.v3 mhost cport)
  (define hostname
    (match mhost
      [(unison-data r t (list host))
       #:when (= t ref-optional-some:tag)
       (chunked-string->string host)]
      [else #f]))

  (define port (chunked-string->string cport))

  (with-handlers
    [[exn:fail:network? left-fail-exn]
     [exn:fail:contract? left-fail-exn]
     [(lambda _ #t) left-fail-k]]

    (ref-either-right
      (tcp-listen
        (string->number port)
        2048
        #t
        (if (equal? "0" hostname) #f hostname)))))

; NOTE: This is a no-op because racket's public TCP stack doesn't have separate operations for
; "bind" vs "listen". We've decided to have `serverSocket` do the "bind & listen", and have
; this do nothing.
; If we want ~a little better parity with the haskell implementation, we might set a flag or
; something on the listener, and error if you try to `accept` on a server socket that you haven't
; called `listen` on yet.
(define-unison-builtin (builtin-IO.listen.impl.v3 _listener)
  (ref-either-right ref-unit-unit))

(define-unison-builtin (builtin-IO.socketAccept.impl.v3 listener)
  (if (socket-pair? listener)
    (ref-either-left
      (ref-failure-failure
        ref-iofailure:typelink
        (string->chunked-string "Cannot accept on a non-server socket")
        (unison-any-any ref-unit-unit)))

    (let-values ([(input output) (tcp-accept listener)])
      (ref-either-right (socket-pair input output)))))

(define-unison-builtin (builtin-Socket.toText s)
  (string->chunked-string (describe-value s)))

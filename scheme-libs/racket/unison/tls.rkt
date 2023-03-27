; TLS primitives! Supplied by openssl (libssl)
#lang racket/base
(require racket/exn
         racket/string
         racket/file
         compatibility/mlist
         unison/data
         unison/tcp
         x509
         openssl)

(provide
 (prefix-out
  unison-FOp-Tls.
  (combine-out
   ClientConfig.default
   ClientConfig.certificates.set
   ServerConfig.default
   decodeCert.impl.v3
   decodePrivateKey
   handshake.impl.v3
   newServer.impl.v3
   newClient.impl.v3
   receive.impl.v3
   send.impl.v3
   terminate.impl.v3)))

(define (decodePrivateKey bytes) ; bytes -> list tlsPrivateKey
  (let* ([tmp (make-temporary-file* #"unison" #".pem")]
         [ctx (ssl-make-server-context)]
         [of (open-output-file tmp #:exists 'replace)])
    (write-bytes bytes of)
    (flush-output of)
    (close-output-port of)
    (with-handlers
        [[exn:fail? (lambda (e) (mlist))]]
      (ssl-load-private-key! ctx tmp)
      (mlist tmp))))

(define (decodeCert.impl.v3 bytes) ; bytes -> either failure tlsSignedCert
  (let ([certs (read-pem-certificates (open-input-bytes bytes))])
    (if (= 1 (length certs))
        (right bytes)
        (exception "Wrong number of certs" "nope" certs))))

(struct server-config (certs key))

(define (ServerConfig.default certs key) ; list tlsSignedCert tlsPrivateKey -> tlsServerConfig
  (server-config certs key))

(struct client-config (host certs))
(struct tls (config input output))

(define (newServer.impl.v3 config sockets) ; tlsServerConfig socket -> {io} tls
  (handle-errors
   (lambda ()
     (let* ([input (socket-pair-input sockets)]
            [output (socket-pair-output sockets)]
            [certs (server-config-certs config)]
            [key (server-config-key config)]
            [tmp (make-temporary-file* #"unison" #".pem")]
            [of (open-output-file tmp #:exists 'replace)])
       (write-bytes (mcar certs) of)
       (flush-output of)
       (close-output-port of)
       (let*-values ([(ctx) (ssl-make-server-context
                             #:private-key (list 'pem key)
                             #:certificate-chain tmp)]
                     [(in out) (ports->ssl-ports
                                input output
                                #:mode 'accept
                                #:context ctx
                                #:close-original? #t
                                )])
         (right (tls config in out)))))))

(define (ClientConfig.default host service-identification-suffix)
  (if (= 0 (bytes-length service-identification-suffix))
      (client-config host (mlist))
      (error 'NotImplemented "service-identification-suffix not supported")))

(define (ClientConfig.certificates.set certs config) ; list tlsSignedCert tlsClientConfig -> tlsClientConfig
  (client-config (client-config-host config) certs))

(define (handle-errors fn)
  (with-handlers
      [[exn:fail:network? (lambda (e) (exception "IOFailure" (exn->string e) '()))]
       [exn:fail:contract? (lambda (e) (exception "InvalidArguments" (exn->string e) '()))]
       [(lambda err
          (string-contains? (exn->string err) "not valid for hostname"))
        (lambda (e) (exception "IOFailure" "NameMismatch" '()))]
       [(lambda _ #t) (lambda (e) (exception "MiscFailure" (format "Unknown exception ~a" (exn->string e)) e))] ]
    (fn)))

(define (newClient.impl.v3 config socket)
  (handle-errors
   (lambda ()
     (let ([input (socket-pair-input socket)]
           [output (socket-pair-output socket)]
           [hostname (client-config-host config)]
           [ctx (ssl-make-client-context)])
       (ssl-set-verify-hostname! ctx #t)
       (let-values ([(in out) (ports->ssl-ports
                               input output
                               #:mode 'connect
                               #:context ctx
                               #:hostname hostname
                               #:close-original? #t
                               )])
         (right (tls config in out)))))))

(define (handshake.impl.v3 tls)
  (handle-errors
   (lambda ()
     (ssl-set-verify! (tls-input tls) #t)
     (right none))))

(define (send.impl.v3 tls data)
  (handle-errors
   (lambda ()
     (let* ([output (tls-output tls)])
       (write-bytes data output)
       (flush-output output)
       (right none)))))

(define (read-more n port)
  (let* ([buffer (make-bytes n)]
         [read (read-bytes-avail! buffer port)])
    (if (< read n)
        (subbytes buffer 0 read)
        (bytes-append buffer (read-more (* 2 n) port)))))

(define (read-all n port)
  (let* ([buffer (make-bytes n)]
         [read (read-bytes-avail! buffer port)])
    (if (= n read)
        (bytes-append buffer (read-more (* 2 n) port))
        (subbytes buffer 0 read))))

(define (receive.impl.v3 tls)
  (handle-errors
   (lambda ()
     (right (read-all 4096 (tls-input tls))))))

(define (terminate.impl.v3 tls)
  ; NOTE: This actually does more than the unison impl,
  ; which only sends the `close_notify` message, and doesn't
  ; mark the port as no longer usable in the runtime.
  ; Not sure if this is an important difference.
  ; Racket's openssl lib doesn't expose a way to *just* call
  ; SSL_Shutdown on a port without also closing it.
  (handle-errors
   (lambda ()
     (ssl-abandon-port (tls-input tls))
     (ssl-abandon-port (tls-output tls))
     (right none))))

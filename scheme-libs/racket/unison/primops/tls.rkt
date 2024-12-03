; TLS primitives! Supplied by openssl (libssl)
#lang racket/base
(require racket/exn
         racket/string
         racket/file
         (only-in racket empty?)
         compatibility/mlist
         unison/boot
         unison/data
         unison/data-info
         unison/chunked-seq
         unison/network-utils
         unison/pem
         x509
         openssl)

(provide
  builtin-Tls.ClientConfig.certificates.set
  builtin-Tls.ClientConfig.certificates.set:termlink
  builtin-Tls.ClientConfig.default
  builtin-Tls.ClientConfig.default:termlink
  builtin-Tls.ServerConfig.default
  builtin-Tls.ServerConfig.default:termlink
  builtin-Tls.decodeCert.impl.v3
  builtin-Tls.decodeCert.impl.v3:termlink
  builtin-Tls.decodePrivateKey
  builtin-Tls.decodePrivateKey:termlink
  builtin-Tls.encodeCert
  builtin-Tls.encodeCert:termlink
  builtin-Tls.encodePrivateKey
  builtin-Tls.encodePrivateKey:termlink
  builtin-Tls.handshake.impl.v3
  builtin-Tls.handshake.impl.v3:termlink
  builtin-Tls.newClient.impl.v3
  builtin-Tls.newClient.impl.v3:termlink
  builtin-Tls.newServer.impl.v3
  builtin-Tls.newServer.impl.v3:termlink
  builtin-Tls.receive.impl.v3
  builtin-Tls.receive.impl.v3:termlink
  builtin-Tls.send.impl.v3
  builtin-Tls.send.impl.v3:termlink
  builtin-Tls.terminate.impl.v3
  builtin-Tls.terminate.impl.v3:termlink)

; Native Representations:
;
; tlsPrivateKey - the "pem" struct defined in pem.rkt
; tlsCertificate - currently the raw bytes

(define (write-to-tmp-file bytes suffix)
  (let* ([tmp (make-temporary-file* #"unison" suffix)]
         [of (open-output-file tmp #:exists 'replace)])
    (write-bytes bytes of)
    (flush-output of)
    (close-output-port of)
    tmp))

(define-unison-builtin (builtin-Tls.encodePrivateKey privateKey)
  (bytes->chunked-bytes
    (string->bytes/utf-8 (pem->pem-string privateKey))))

; bytes -> list tlsPrivateKey
(define-unison-builtin (builtin-Tls.decodePrivateKey bytes)
  (vector->chunked-list
    (list->vector ; TODO better conversion
      (filter
        (lambda (pem) (or
                       (equal? "PRIVATE KEY" (pem-label pem))
                       (equal? "RSA PRIVATE KEY" (pem-label pem))))

        (pem-string->pems
          (bytes->string/utf-8 (chunked-bytes->bytes bytes)))))))

; bytes -> either failure tlsSignedCert
(define-unison-builtin (builtin-Tls.decodeCert.impl.v3 bytes)
  (define certs
    (read-pem-certificates
      (open-input-bytes (chunked-bytes->bytes bytes))))

  (if (= 1 (length certs))
      (ref-either-right bytes)
      (ref-either-left
        (ref-failure-failure
          ref-tlsfailure:typelink
          (string->chunked-string "Could not decode certificate")
          (unison-any-any bytes)))))

; We don't actually "decode" certificates, we just validate them
(define-unison-builtin (builtin-Tls.encodeCert bytes) bytes)

(struct server-config (certs key)) ; certs = list certificate; key = privateKey

(define-unison-builtin
  ; list tlsSignedCert tlsPrivateKey -> tlsServerConfig
  (builtin-Tls.ServerConfig.default certs key)
  (server-config certs key))

(struct client-config (host certs))
(struct tls (config input output))

; tlsServerConfig socket -> {io} tls
(define-unison-builtin (builtin-Tls.newServer.impl.v3 config socket-pair)
  (handle-errors
    (let* ([input (socket-pair-input socket-pair)]
           [output (socket-pair-output socket-pair)]
           [certs (server-config-certs config)]
           [key (server-config-key config)]
           [key-bytes (string->bytes/utf-8 (pem->pem-string key))]
           [tmp (write-to-tmp-file (chunked-bytes->bytes (chunked-list-ref certs 0)) #".pem")])
      (let*-values ([(ctx) (ssl-make-server-context
                            ; TODO: Once racket can handle the in-memory PEM bytes,
                            ; we can do away with writing them out to temporary files.
                            ; https://github.com/racket/racket/pull/4625
                            ; #:private-key (list 'pem key-bytes)
                            #:private-key (list 'pem (write-to-tmp-file key-bytes #".pem"))
                            #:certificate-chain tmp)]
                    [(in out) (ports->ssl-ports
                               input output
                               #:mode 'accept
                               #:context ctx
                               #:close-original? #t
                               )])
        (ref-either-right (tls config in out))))))

(define-unison-builtin
  ; string bytes
  (builtin-Tls.ClientConfig.default host service-identification-suffix)
  (if (= 0 (chunked-bytes-length service-identification-suffix))
      (client-config host empty-chunked-list)
      ; todo: better error?
      (error 'NotImplemented
             "service-identification-suffix not supported")))

(define (ServerConfig.certificates.set certs config)
  (server-config certs (server-config-key config)))

(define-unison-builtin
  ; list tlsSignedCert tlsClientConfig -> tlsClientConfig
  (builtin-Tls.ClientConfig.certificates.set certs config)
  (client-config (client-config-host config) certs))

(define (left-fail ty msg val)
  (ref-either-left
    (ref-failure-failure
      ty
      (string->chunked-string msg)
      (unison-any-any val))))

(define ((left-fail-exn ty) e)
  (left-fail ty (exn->string e) ref-unit-unit))

(define ((left-fail-k ty msg) e)
  (left-fail ty msg ref-unit-unit))

(define (exn:name-mismatch? e)
  (string-contains? (exn->string e) "not valid for hostname"))

(define (exn:cert-verify? e)
  (string-contains? (exn->string e) "certificate verify failed"))

(define-syntax handle-errors
  (syntax-rules ()
    [(handle-errors ex ...)
     (with-handlers
       [[exn:fail:network? (left-fail-exn ref-iofailure:typelink)]
        [exn:fail:contract? (left-fail-exn ref-miscfailure:typelink)]
        [exn:name-mismatch?
          (left-fail-k ref-tlsfailure:typelink "NameMismatch")]
        [exn:cert-verify?
          (left-fail-k ref-tlsfailure:typelink
                       "certificate verify failed")]
        [(lambda _ #t)
         (lambda (e)
           (left-fail
             ref-miscfailure:typelink
             (format "Unknown exception ~a" (exn->string e))
             ref-unit-unit))]]
       ex ...)]))

(define-unison-builtin (builtin-Tls.newClient.impl.v3 config socket)
  (handle-errors
    (let* ([input (socket-pair-input socket)]
           [output (socket-pair-output socket)]
           [hostname (client-config-host config)]
           ; TODO: Make the client context up in ClientConfig.default
           ; instead of right here.
           [ctx (ssl-make-client-context)]
           [certs (client-config-certs config)])
      (ssl-set-verify-hostname! ctx #t)
      (ssl-set-ciphers! ctx "DEFAULT:!aNULL:!eNULL:!LOW:!EXPORT:!SSLv2")
      (ssl-set-verify! ctx #t)
      (if (chunked-list-empty? certs)
        (ssl-load-default-verify-sources! ctx)
        (let ([tmp (write-to-tmp-file (chunked-bytes->bytes (chunked-list-ref certs 0)) #".pem")])
           (ssl-load-verify-source! ctx tmp)))
      (let-values ([(in out) (ports->ssl-ports
                              input output
                              #:mode 'connect
                              #:context ctx
                              #:hostname (chunked-string->string hostname)
                              #:close-original? #t
                              )])
        (ref-either-right (tls config in out))))))

(define-unison-builtin (builtin-Tls.handshake.impl.v3 tls)
  (handle-errors
    (ssl-set-verify! (tls-input tls) #t)
    (ref-either-right ref-unit-unit)))

; data = bytes
(define-unison-builtin (builtin-Tls.send.impl.v3 tls data)
  (handle-errors
    (let* ([output (tls-output tls)])
      (write-bytes (chunked-bytes->bytes data) output)
      (flush-output output)
      (ref-either-right ref-unit-unit))))

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

; -> bytes
(define-unison-builtin (builtin-Tls.receive.impl.v3 tls)
  (handle-errors
    (ref-either-right
      (bytes->chunked-bytes (read-all 4096 (tls-input tls))))))

(define-unison-builtin (builtin-Tls.terminate.impl.v3 tls)
  ; NOTE: This actually does more than the unison impl,
  ; which only sends the `close_notify` message, and doesn't
  ; mark the port as no longer usable in the runtime.
  ; Not sure if this is an important difference.
  ; Racket's openssl lib doesn't expose a way to *just* call
  ; SSL_Shutdown on a port without also closing it.
  (handle-errors
    (ssl-abandon-port (tls-input tls))
    (ssl-abandon-port (tls-output tls))
    (ref-either-right ref-unit-unit)))


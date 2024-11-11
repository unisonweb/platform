#lang racket/base

; This implements a standalone unison runtime, with options for
; generating compilable racket modules.
;
; For runtime, it relies on the support for unison dynamic code
; loading. It expects to be provided with a serialized list of term
; links and associated code. It then loads the code in the same manner
; as dynamic runtime execution, and evaluates a main definition.
;
; Since this is intended to be an implementation of evaluation for
; e.g. ucm, the input is expected to be complete. No protocol is
; implemented for negotiating with a host for additional needed
; definitions. The program has all the built in definitions, and
; everything else is expected to be provided in the initial input.
;
; In addition to this mode, it is possible to supply a command line
; argument `-G` with a file name. This will instead produce a racket
; file with the supplied definitions. This file should be suitable for
; compilation and distribution with the `raco` tool, so long as the
; supporting unison-on-racket libraries are known to the racket
; install.

(require
  racket/pretty
  (except-in racket false true unit any)
  compiler/embed
  unison/boot
  unison/data
  unison/data-info
  unison/chunked-seq
  unison/primops
  unison/primops-generated
  unison/builtin-generated)

(define (grab-num port)
  (integer-bytes->integer (read-bytes 4 port) #f #t 0 4))

; Gets bytes using the expected input format. The format is simple:
;
;  - 4 bytes indicating how many bytes follow
;  - the actual payload, with size matching the above
(define (grab-bytes port)
  (let ([size (grab-num port)])
    (read-bytes size port)))

; Gets args sent after the code payload. Format is:
;
; - 4 bytes indicating how many arguments
; - for each argument
;   - 4 bytes indicating length of argument
;   - utf-8 bytes of that length
(define (grab-args port)
  (let ([n (grab-num port)])
    (for/list ([i (range n)])
      (bytes->string/utf-8 (grab-bytes port)))))

; Reads and decodes the input. First uses `grab-bytes` to read the
; payload, then uses unison functions to deserialize the `Value` that
; is expected.
;
; The `Value` is expected to be a pair of loadable code and which
; definition should be executed. In unison types, it is:
;
;   ([(Link.Term, Code)], Link.Term)
(define (decode-input port)
  (let ([bs (grab-bytes port)])
    (match (builtin-Value.deserialize (bytes->chunked-bytes bs))
      [(unison-data _ t (list q))
       #:when (= t ref-either-right:tag)
       (apply
         values
         (unison-tuple->list (reify-value (unison-quote-val q))))]
      [val
        (raise (format "unexpected input: ~a " (describe-value val)))])))

(define (natural->bytes/variable n)
  (let rec ([i n] [acc '()])
    (cond
      [(< i #x80) (list->bytes (reverse (cons i acc)))]
      [else
        (rec (arithmetic-shift i -7)
             (cons (bitwise-and i #x7f) acc))])))

(define (write-string-bytes str port)
  (define bs (string->bytes/utf-8 str))
  (write-bytes (natural->bytes/variable (bytes-length bs)) port)
  (write-bytes bs port))

(define (write-value-bytes val port)
  (define qval (unison-quote (reflect-value val)))
  (define bs (chunked-bytes->bytes (builtin-Value.serialize qval)))
  (write-bytes bs port))

(define (encode-success result port)
  (write-bytes #"\0" port)
  (write-value-bytes result port)
  (void))

(define (encode-error ex port)
  (match ex
    [(exn:bug msg val)
     (write-bytes #"\1" port)
     (write-string-bytes msg port)
     (write-value-bytes val port)]
    [else
     (write-bytes #"\2" port)
     (write-string-bytes (exception->string ex) port)])
  (void))

(define (encode-exception fail port)
  (write-bytes #"\1" port)
  (write-string-bytes "builtin.raise" port)
  (write-value-bytes fail port)
  (void))

(define ((eval-exn-handler port) rq)
  (request-case rq
    [pure (result) (encode-success result port)]
    [ref-exception
      [0 (fail)
        (control ref-exception k
          (encode-exception fail port))]]))

; Implements the evaluation mode of operation. First decodes the
; input. Then uses the dynamic loading machinery to add the code to
; the runtime. Finally executes a specified main reference.
(define (do-evaluate in out)
  (let-values ([(code main-ref) (decode-input in)]
               [(args) (list->vector (grab-args in))])
    (add-runtime-code 'unison-main code)
    (with-handlers
      ([exn:bug? (lambda (e) (encode-error e out))])

      (parameterize ([current-command-line-arguments args])
        (handle [ref-exception] (eval-exn-handler out)
                ((termlink->proc main-ref)))))))

; Uses racket pretty printing machinery to instead generate a file
; containing the given code, and which executes the main definition on
; loading. This file can then be built with `raco exe`.
(define (write-module prof srcf main-ref icode)
  (call-with-output-file
    srcf
    (lambda (port)
      (parameterize ([print-as-expression #t])
        (display "#lang racket/base\n\n" port)

        (for ([expr (build-intermediate-module #:profile prof main-ref icode)])
          (pretty-print expr port 1)
          (newline port))
        (newline port)))
    #:exists 'replace))

; Decodes input and writes a module to the specified file.
(define (do-generate prof srcf)
  (define-values (icode main-ref) (decode-input (current-input-port)))
  (write-module prof srcf main-ref icode))

(define generate-to (make-parameter #f))
(define show-version (make-parameter #f))
(define use-port-num (make-parameter #f))
(define enable-profiling (make-parameter #f))

(define (handle-command-line)
  (command-line
    #:program "unison-runtime"
    #:once-any
    ["--version"
     "display version"
     (show-version #t)]
    [("-p" "--port")
     port-num
     "runtime communication port"
     (use-port-num port-num)]
    [("-G" "--generate-file")
       file
       "generate code to <file>"
       (generate-to file)]
    #:once-each
    [("--profile")
     "enable profiling"
     (enable-profiling #t)]
    #:args remaining
    (list->vector remaining)))

(begin
  (let ([sub-args (handle-command-line)])
    (current-command-line-arguments sub-args))
  (cond
    [(show-version) (displayln "unison-runtime version 0.0.11")]
    [(generate-to) (do-generate (enable-profiling) (generate-to))]
    [(use-port-num)
     (match (string->number (use-port-num))
       [port
        #:when (port-number? port)
        (let-values ([(in out) (tcp-connect "localhost" port)])
          (do-evaluate in out)
          (close-output-port out)
          (close-input-port in))]
       [#f
        (displayln "could not parse port number")
        (exit 1)]
       [port
        (displayln "bad port number")
        (exit 1)])]
    [else
      (do-evaluate (current-input-port) (open-output-bytes))]))

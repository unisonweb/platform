#lang racket/base

(require racket/string
         rnrs/io/ports-6
         (only-in rnrs standard-error-port standard-input-port standard-output-port)
         (only-in racket
                  empty?
                  match
                  with-output-to-string
                  system/exit-code
                  system
                  false?)
         unison/boot
         unison/data
         unison/chunked-seq
         unison/data
         unison/data-info
         unison/chunked-seq
         unison/data
         )

(provide
  builtin-Handle.toText
  builtin-Handle.toText:termlink

  builtin-IO.closeFile.impl.v3
  builtin-IO.closeFile.impl.v3:termlink
  builtin-IO.getBytes.impl.v3
  builtin-IO.getBytes.impl.v3:termlink
  builtin-IO.stdHandle
  builtin-IO.stdHandle:termlink
  builtin-IO.openFile.impl.v3
  builtin-IO.openFile.impl.v3:termlink
  builtin-IO.putBytes.impl.v3
  builtin-IO.putBytes.impl.v3:termlink
  builtin-IO.seekHandle.impl.v3
  builtin-IO.seekHandle.impl.v3:termlink
  builtin-IO.getLine.impl.v1
  builtin-IO.getLine.impl.v1:termlink
  builtin-IO.getSomeBytes.impl.v1
  builtin-IO.getSomeBytes.impl.v1:termlink
  builtin-IO.getBuffering.impl.v3
  builtin-IO.getBuffering.impl.v3:termlink
  builtin-IO.setBuffering.impl.v3
  builtin-IO.setBuffering.impl.v3:termlink
  builtin-IO.getEcho.impl.v1
  builtin-IO.getEcho.impl.v1:termlink
  builtin-IO.setEcho.impl.v1
  builtin-IO.setEcho.impl.v1:termlink
  builtin-IO.getChar.impl.v1
  builtin-IO.getChar.impl.v1:termlink
  builtin-IO.isFileOpen.impl.v3
  builtin-IO.isFileOpen.impl.v3:termlink
  builtin-IO.isSeekable.impl.v3
  builtin-IO.isSeekable.impl.v3:termlink
  builtin-IO.handlePosition.impl.v3
  builtin-IO.handlePosition.impl.v3:termlink
  builtin-IO.process.call
  builtin-IO.process.call:termlink
  builtin-IO.ready.impl.v1
  builtin-IO.ready.impl.v1:termlink

; Still to implement:
;    handlePosition.impl.v3
;    isSeekable.impl.v3
;    getChar.impl.v1
   )

; typeLink msg any
(define (Exception typeLink message payload)
    (let* ([a (unison-any-any payload)]
           [msg (string->chunked-string message)]
           [f (ref-failure-failure typeLink msg a)])
    (ref-either-left f)))

(define-unison-builtin
  (builtin-IO.isFileOpen.impl.v3 port)
    (ref-either-right (not (port-closed? port))))

(define-unison-builtin
  (builtin-IO.ready.impl.v1 port)
    (if (byte-ready? port)
        (ref-either-right #t)
        (if (port-eof? port)
            (Exception ref-iofailure:typelink "EOF" port)
            (ref-either-right #f))))

(define-unison-builtin
  (builtin-IO.isSeekable.impl.v3 handle)
    (ref-either-right
        (port-has-set-port-position!? handle)))

(define-unison-builtin
  (builtin-IO.handlePosition.impl.v3 handle)
    (ref-either-right (port-position handle)))

(define-unison-builtin
  (builtin-IO.seekHandle.impl.v3 handle mode amount)
    (data-case mode
        (0 ()
            (set-port-position! handle amount)
            (ref-either-right none))
        (1 ()
            (let ([current (port-position handle)])
                (set-port-position! handle (+ current amount))
                (ref-either-right none)))
        (2 ()
            (Exception
              ref-iofailure:typelink
              "SeekFromEnd not supported"
              0))))

(define-unison-builtin
  (builtin-IO.getLine.impl.v1 handle)
  (let* ([line (read-line handle)])
    (if (eof-object? line)
        (ref-either-right (string->chunked-string ""))
        (ref-either-right (string->chunked-string line))
        )))

(define-unison-builtin
  (builtin-IO.getChar.impl.v1 handle)
  (let* ([char (read-char handle)])
    (if (eof-object? char)
        (Exception
          ref-iofailure:typelink
          "End of file reached"
          ref-unit-unit)
        (ref-either-right char))))

(define-unison-builtin
  (builtin-IO.getSomeBytes.impl.v1 handle nbytes)
  (let* ([buffer (make-bytes nbytes)]
         [line (read-bytes-avail! buffer handle)])
    (cond
      [(eof-object? line)
       (ref-either-right (bytes->chunked-bytes #""))]
      [(procedure? line)
       (Exception
         ref-iofailure:typelink
         "getSomeBytes.impl: special value returned"
         ref-unit-unit)]
      [else
       (ref-either-right
         (bytes->chunked-bytes
           (if (< line nbytes)
             (subbytes buffer 0 line)
             buffer)))])))

(define-unison-builtin
  (builtin-IO.getBuffering.impl.v3 handle)
    (case (file-stream-buffer-mode handle)
        [(none) (ref-either-right ref-buffermode-no-buffering)]
        [(line) (ref-either-right
                  ref-buffermode-line-buffering)]
        [(block) (ref-either-right
                   ref-buffermode-block-buffering)]
        [(#f) (Exception
                ref-iofailure:typelink
                "Unable to determine buffering mode of handle"
                ref-unit-unit)]
        [else (Exception
                ref-iofailure:typelink
                "Unexpected response from file-stream-buffer-mode"
                ref-unit-unit)]))

(define-unison-builtin
  (builtin-IO.setBuffering.impl.v3 handle mode)
    (data-case mode
        (0 ()
            (file-stream-buffer-mode handle 'none)
            (ref-either-right none))
        (1 ()
            (file-stream-buffer-mode handle 'line)
            (ref-either-right none))
        (2 ()
            (file-stream-buffer-mode handle 'block)
            (ref-either-right none))
        (3 (size)
            (Exception
              ref-iofailure:typelink
              "Sized block buffering not supported"
              ref-unit-unit))))

(define (with-buffer-mode port mode)
  (file-stream-buffer-mode port mode)
  port)

(define stdin (with-buffer-mode (standard-input-port) 'none))
(define stdout (with-buffer-mode (standard-output-port) 'line))
(define stderr (with-buffer-mode (standard-error-port) 'line))

(define-unison-builtin (builtin-IO.stdHandle sth)
  (match sth
    [(unison-data r t (list))
     (=> break)
     (cond
       [(= t ref-stdhandle-stdin:tag) stdin]
       [(= t ref-stdhandle-stdout:tag) stdout]
       [(= t ref-stdhandle-stderr:tag) stderr]
       [else (break)])]
    [else
     (raise (make-exn:bug "invalid standard handle" sth))]))

(define-unison-builtin
  (builtin-IO.getEcho.impl.v1 handle)
  (if (eq? handle stdin)
      (ref-either-right (get-stdin-echo))
      (Exception
        ref-iofailure:typelink
        "getEcho only supported on stdin"
        ref-unit-unit)))

(define-unison-builtin
  (builtin-IO.setEcho.impl.v1 handle echo)
  (if (eq? handle stdin)
      (begin
        (if echo
            (system "stty echo")
            (system "stty -echo"))
        (ref-either-right none))
      (Exception
        ref-iofailure:typelink
        "setEcho only supported on stdin"
        ref-unit-unit)))

(define (get-stdin-echo)
  (let ([current (with-output-to-string (lambda () (system "stty -a")))])
    (string-contains? current " echo ")))

(define-unison-builtin (builtin-IO.openFile.impl.v3 name mode)
  (define fn (chunked-string->string name))

  (match mode
    [(unison-data r t _)
     (=> break)
     (ref-either-right
       (cond
         [(= t ref-filemode-read:tag)
          (open-input-file fn)]
         [(= t ref-filemode-write:tag)
          (open-output-file fn #:exists 'truncate)]
         [(= t ref-filemode-append:tag)
          (open-output-file fn #:exists 'append)]
         [(= t ref-filemode-readwrite:tag)
          (open-input-output-file fn #:exists 'can-update)]
         ; break back to outer match
         [else (break)]))]
    [else
      (ref-either-left
        (ref-failure-failure
          ref-iofailure:typelink
          (string->chunked-string "invalid file mode")
          (unison-any-any mode)))]))

;; From https://github.com/sorawee/shlex/blob/5de06500e8c831cfc8dffb99d57a76decc02c569/main.rkt (MIT License)
;; with is a port of https://github.com/python/cpython/blob/bf2f76ec0976c09de79c8827764f30e3b6fba776/Lib/shlex.py#L325
(define unsafe-pattern #rx"[^a-zA-Z0-9_@%+=:,./-]")
(define (quote-arg s)
  (if (non-empty-string? s)
      (if (regexp-match unsafe-pattern s)
          (string-append "'" (string-replace s "'" "'\"'\"'") "'")
          s)
      "''"))

(define-unison-builtin
  (builtin-IO.process.call command arguments)
  (system/exit-code
          (string-join (cons
                        (chunked-string->string command)
                        (map (lambda (arg) (quote-arg (chunked-string->string arg)))
                        (vector->list
                             (chunked-list->vector arguments))))
                             " ")))

(define-unison-builtin (builtin-Handle.toText h)
  (string->chunked-string (describe-value h)))

(define-unison-builtin (builtin-IO.getBytes.impl.v3 h n)
  (with-handlers
    ; TODO: seems like we should catch more
    [[exn:fail:contract?
       (lambda (e)
         (ref-either-left
           (ref-failure-failure
             ref-iofailure:typelink
             (exception->string e)
             ref-unit-unit)))]]
    (ref-either-right
      (bytes->chunked-bytes
        (read-bytes n h)))))

(define-unison-builtin (builtin-IO.putBytes.impl.v3 h bs)
  ; TODO: error checking?
  (write-bytes (chunked-bytes->bytes bs) h)
  (flush-output h)
  (ref-either-right ref-unit-unit))

(define-unison-builtin (builtin-IO.closeFile.impl.v3 h)
  ; TODO: review this implementation; moved from primops.ss
  (if (input-port? h)
    (close-input-port h)
    (close-output-port h))
  (ref-either-right ref-unit-unit))


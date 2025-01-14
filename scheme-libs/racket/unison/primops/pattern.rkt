#lang racket/base

(require (except-in unison/boot control)
         unison/data
         unison/data-info
         unison/pattern)

(provide
  builtin-Char.Class.alphanumeric
  builtin-Char.Class.alphanumeric:termlink
  builtin-Char.Class.and
  builtin-Char.Class.and:termlink
  builtin-Char.Class.any
  builtin-Char.Class.any:termlink
  builtin-Char.Class.anyOf
  builtin-Char.Class.anyOf:termlink
  builtin-Char.Class.control
  builtin-Char.Class.control:termlink
  builtin-Char.Class.letter
  builtin-Char.Class.letter:termlink
  builtin-Char.Class.lower
  builtin-Char.Class.lower:termlink
  builtin-Char.Class.mark
  builtin-Char.Class.mark:termlink
  builtin-Char.Class.not
  builtin-Char.Class.not:termlink
  builtin-Char.Class.number
  builtin-Char.Class.number:termlink
  builtin-Char.Class.or
  builtin-Char.Class.or:termlink
  builtin-Char.Class.printable
  builtin-Char.Class.printable:termlink
  builtin-Char.Class.punctuation
  builtin-Char.Class.punctuation:termlink
  builtin-Char.Class.range
  builtin-Char.Class.range:termlink
  builtin-Char.Class.separator
  builtin-Char.Class.separator:termlink
  builtin-Char.Class.symbol
  builtin-Char.Class.symbol:termlink
  builtin-Char.Class.upper
  builtin-Char.Class.upper:termlink
  builtin-Char.Class.whitespace
  builtin-Char.Class.whitespace:termlink

  builtin-Pattern.capture
  builtin-Pattern.capture:termlink
  builtin-Pattern.join
  builtin-Pattern.join:termlink
  builtin-Pattern.many
  builtin-Pattern.many:termlink
  builtin-Pattern.or
  builtin-Pattern.or:termlink
  builtin-Pattern.replicate
  builtin-Pattern.replicate:termlink
  builtin-Pattern.run
  builtin-Pattern.run:termlink

  builtin-Char.Class.is
  builtin-Char.Class.is:termlink
  builtin-Pattern.captureAs
  builtin-Pattern.captureAs:termlink
  builtin-Pattern.many.corrected
  builtin-Pattern.many.corrected:termlink
  builtin-Pattern.isMatch
  builtin-Pattern.isMatch:termlink

  builtin-Text.patterns.anyChar
  builtin-Text.patterns.anyChar:termlink
  builtin-Text.patterns.char
  builtin-Text.patterns.char:termlink
  builtin-Text.patterns.charIn
  builtin-Text.patterns.charIn:termlink
  builtin-Text.patterns.charRange
  builtin-Text.patterns.charRange:termlink
  builtin-Text.patterns.digit
  builtin-Text.patterns.digit:termlink
  builtin-Text.patterns.eof
  builtin-Text.patterns.eof:termlink
  builtin-Text.patterns.letter
  builtin-Text.patterns.letter:termlink
  builtin-Text.patterns.literal
  builtin-Text.patterns.literal:termlink
  builtin-Text.patterns.notCharIn
  builtin-Text.patterns.notCharIn:termlink
  builtin-Text.patterns.notCharRange
  builtin-Text.patterns.notCharRange:termlink
  builtin-Text.patterns.punctuation
  builtin-Text.patterns.punctuation:termlink
  builtin-Text.patterns.space
  builtin-Text.patterns.space:termlink)


(define-unison-builtin #:hints [value] (builtin-Char.Class.alphanumeric)
  alphanumeric)

(define-unison-builtin (builtin-Char.Class.and l r)
  (char-class-and l r))

(define-unison-builtin #:hints [value] (builtin-Char.Class.any)
  any-char)

(define-unison-builtin (builtin-Char.Class.anyOf cs)
  (chars cs))

(define-unison-builtin #:hints [value] (builtin-Char.Class.control)
  control)

(define-unison-builtin #:hints [value] (builtin-Char.Class.letter)
  letter)

(define-unison-builtin #:hints [value] (builtin-Char.Class.lower)
  lower)

(define-unison-builtin #:hints [value] (builtin-Char.Class.mark)
  mark)

(define-unison-builtin (builtin-Char.Class.not c)
  (char-class-not c))

(define-unison-builtin #:hints [value] (builtin-Char.Class.number)
  number)

(define-unison-builtin (builtin-Char.Class.or l r)
  (char-class-or l r))

(define-unison-builtin #:hints [value] (builtin-Char.Class.printable)
  printable)

(define-unison-builtin #:hints [value] (builtin-Char.Class.punctuation)
  punctuation)

(define-unison-builtin (builtin-Char.Class.range l u)
  (char-range l u))

(define-unison-builtin #:hints [value] (builtin-Char.Class.separator)
  separator)

(define-unison-builtin #:hints [value] (builtin-Char.Class.symbol)
  symbol)

(define-unison-builtin #:hints [value] (builtin-Char.Class.upper)
  upper)

(define-unison-builtin #:hints [value] (builtin-Char.Class.whitespace)
  space)


(define-unison-builtin (builtin-Pattern.capture p) (capture p))

(define-unison-builtin (builtin-Pattern.join ps) (join* ps))

(define-unison-builtin (builtin-Pattern.many p) (many p))

(define-unison-builtin (builtin-Pattern.or l r) (choice l r))

(define-unison-builtin (builtin-Pattern.replicate m n p)
  (replicate p m n))

(define-unison-builtin (builtin-Pattern.run p t)
  (let ([r (pattern-match p t)])
    (if r
      (ref-optional-some (unison-tuple (cdr r) (car r)))
      ref-optional-none)))


(define-unison-builtin #:hints [value] (builtin-Text.patterns.anyChar)
  any-char)

(define-unison-builtin (builtin-Text.patterns.char cc) cc)

(define-unison-builtin (builtin-Text.patterns.charIn cs)
  (chars cs))

(define-unison-builtin (builtin-Text.patterns.charRange c d)
  (char-range c d))

(define-unison-builtin #:hints [value] (builtin-Text.patterns.digit)
  digit)

(define-unison-builtin #:hints [value] (builtin-Text.patterns.eof)
  eof)

(define-unison-builtin #:hints [value] (builtin-Text.patterns.letter)
  letter)

(define-unison-builtin (builtin-Text.patterns.literal t)
  (literal t))

(define-unison-builtin (builtin-Text.patterns.notCharIn cs)
  (not-chars cs))

(define-unison-builtin (builtin-Text.patterns.notCharRange c d)
  (not-char-range c d))

(define-unison-builtin #:hints [value]
                       (builtin-Text.patterns.punctuation)
  punctuation)

(define-unison-builtin #:hints [value] (builtin-Text.patterns.space)
  space)

(define-unison-builtin (builtin-Char.Class.is cc c)
  (pattern-match? cc (string->chunked-string (string c))))

(define-unison-builtin (builtin-Pattern.captureAs c p)
  (capture-as c p))

(define-unison-builtin (builtin-Pattern.many.corrected p) (many p))

(define-unison-builtin (builtin-Pattern.isMatch p s)
  (pattern-match? p s))


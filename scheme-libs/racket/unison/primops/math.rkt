#lang racket/base

(require unison/boot
         unison/chunked-seq
         unison/data
         unison/data-info

         (except-in math/base sum)

         racket/fixnum
         racket/flonum

         (only-in racket/string
                  string-contains?
                  string-replace)

         (only-in rnrs/arithmetic/bitwise-6
                  bitwise-bit-count
                  bitwise-first-bit-set))

(provide
  builtin-Float.+
  builtin-Float.+:termlink
  builtin-Float.*
  builtin-Float.*:termlink
  builtin-Float.-
  builtin-Float.-:termlink
  builtin-Float./
  builtin-Float./:termlink
  builtin-Float.>=
  builtin-Float.>=:termlink
  builtin-Float.<=
  builtin-Float.<=:termlink
  builtin-Float.>
  builtin-Float.>:termlink
  builtin-Float.<
  builtin-Float.<:termlink
  builtin-Float.==
  builtin-Float.==:termlink
  builtin-Float.abs
  builtin-Float.abs:termlink
  builtin-Float.acos
  builtin-Float.acos:termlink
  builtin-Float.acosh
  builtin-Float.acosh:termlink
  builtin-Float.asin
  builtin-Float.asin:termlink
  builtin-Float.asinh
  builtin-Float.asinh:termlink
  builtin-Float.atan
  builtin-Float.atan:termlink
  builtin-Float.atan2
  builtin-Float.atan2:termlink
  builtin-Float.atanh
  builtin-Float.atanh:termlink
  builtin-Float.cos
  builtin-Float.cos:termlink
  builtin-Float.cosh
  builtin-Float.cosh:termlink
  builtin-Float.fromText
  builtin-Float.fromText:termlink
  builtin-Float.sin
  builtin-Float.sin:termlink
  builtin-Float.sinh
  builtin-Float.sinh:termlink
  builtin-Float.toText
  builtin-Float.toText:termlink
  builtin-Float.truncate
  builtin-Float.truncate:termlink
  builtin-Float.exp
  builtin-Float.exp:termlink
  builtin-Float.fromRepresentation
  builtin-Float.fromRepresentation:termlink
  builtin-Float.log
  builtin-Float.log:termlink
  builtin-Float.max
  builtin-Float.max:termlink
  builtin-Float.min
  builtin-Float.min:termlink
  builtin-Float.pow
  builtin-Float.pow:termlink
  builtin-Float.sqrt
  builtin-Float.sqrt:termlink
  builtin-Float.tan
  builtin-Float.tan:termlink
  builtin-Float.tanh
  builtin-Float.tanh:termlink
  builtin-Float.toRepresentation
  builtin-Float.toRepresentation:termlink
  builtin-Float.logBase
  builtin-Float.logBase:termlink
  builtin-Float.fromRepresentation
  builtin-Float.fromRepresentation:termlink
  builtin-Float.toRepresentation
  builtin-Float.toRepresentation:termlink
  builtin-Float.ceiling
  builtin-Float.ceiling:termlink


  builtin-Int.+
  builtin-Int.+:termlink
  builtin-Int.*
  builtin-Int.*:termlink
  builtin-Int.-
  builtin-Int.-:termlink
  builtin-Int./
  builtin-Int./:termlink
  builtin-Int.==
  builtin-Int.==:termlink
  builtin-Int.<
  builtin-Int.<:termlink
  builtin-Int.<=
  builtin-Int.<=:termlink
  builtin-Int.>
  builtin-Int.>:termlink
  builtin-Int.>=
  builtin-Int.>=:termlink
  builtin-Int.and
  builtin-Int.and:termlink
  builtin-Int.complement
  builtin-Int.complement:termlink
  builtin-Int.fromRepresentation
  builtin-Int.fromRepresentation:termlink
  builtin-Int.fromText
  builtin-Int.fromText:termlink
  builtin-Int.increment
  builtin-Int.increment:termlink
  builtin-Int.isEven
  builtin-Int.isEven:termlink
  builtin-Int.isOdd
  builtin-Int.isOdd:termlink
  builtin-Int.leadingZeros
  builtin-Int.leadingZeros:termlink
  builtin-Int.mod
  builtin-Int.mod:termlink
  builtin-Int.negate
  builtin-Int.negate:termlink
  builtin-Int.or
  builtin-Int.or:termlink
  builtin-Int.popCount
  builtin-Int.popCount:termlink
  builtin-Int.shiftLeft
  builtin-Int.shiftLeft:termlink
  builtin-Int.shiftRight
  builtin-Int.shiftRight:termlink
  builtin-Int.signum
  builtin-Int.signum:termlink
  builtin-Int.toFloat
  builtin-Int.toFloat:termlink
  builtin-Int.toRepresentation
  builtin-Int.toRepresentation:termlink
  builtin-Int.toText
  builtin-Int.toText:termlink
  builtin-Int.truncate0
  builtin-Int.truncate0:termlink
  builtin-Int.xor
  builtin-Int.xor:termlink
  builtin-Int.pow
  builtin-Int.pow:termlink
  builtin-Int.trailingZeros
  builtin-Int.trailingZeros:termlink


  builtin-Nat.+
  builtin-Nat.+:termlink
  builtin-Nat.*
  builtin-Nat.*:termlink
  builtin-Nat./
  builtin-Nat./:termlink
  builtin-Nat.==
  builtin-Nat.==:termlink
  builtin-Nat.<
  builtin-Nat.<:termlink
  builtin-Nat.<=
  builtin-Nat.<=:termlink
  builtin-Nat.>
  builtin-Nat.>:termlink
  builtin-Nat.>=
  builtin-Nat.>=:termlink
  builtin-Nat.and
  builtin-Nat.and:termlink
  builtin-Nat.complement
  builtin-Nat.complement:termlink
  builtin-Nat.drop
  builtin-Nat.drop:termlink
  builtin-Nat.increment
  builtin-Nat.increment:termlink
  builtin-Nat.isEven
  builtin-Nat.isEven:termlink
  builtin-Nat.isOdd
  builtin-Nat.isOdd:termlink
  builtin-Nat.fromText
  builtin-Nat.fromText:termlink
  builtin-Nat.leadingZeros
  builtin-Nat.leadingZeros:termlink
  builtin-Nat.mod
  builtin-Nat.mod:termlink
  builtin-Nat.or
  builtin-Nat.or:termlink
  builtin-Nat.popCount
  builtin-Nat.popCount:termlink
  builtin-Nat.pow
  builtin-Nat.pow:termlink
  builtin-Nat.shiftLeft
  builtin-Nat.shiftLeft:termlink
  builtin-Nat.shiftRight
  builtin-Nat.shiftRight:termlink
  builtin-Nat.sub
  builtin-Nat.sub:termlink
  builtin-Nat.toFloat
  builtin-Nat.toFloat:termlink
  builtin-Nat.toInt
  builtin-Nat.toInt:termlink
  builtin-Nat.toText
  builtin-Nat.toText:termlink
  builtin-Nat.trailingZeros
  builtin-Nat.trailingZeros:termlink
  builtin-Nat.xor
  builtin-Nat.xor:termlink)


(define-unison-builtin (builtin-Float.* x y) (fl* x y))

(define-unison-builtin (builtin-Float.+ x y) (fl+ x y))

(define-unison-builtin (builtin-Float.- x y) (fl- x y))

(define-unison-builtin (builtin-Float./ x y) (fl/ x y))

(define-unison-builtin (builtin-Float.> x y) (fl> x y))

(define-unison-builtin (builtin-Float.< x y) (fl< x y))

(define-unison-builtin (builtin-Float.>= x y) (fl>= x y))

(define-unison-builtin (builtin-Float.<= x y) (fl<= x y))

(define-unison-builtin (builtin-Float.== x y) (fl= x y))

(define-unison-builtin (builtin-Float.abs x) (flabs x))

(define-unison-builtin (builtin-Float.acos x) (flacos x))

(define-unison-builtin (builtin-Float.acosh x) (acosh x))

(define-unison-builtin (builtin-Float.asin x) (flasin x))

(define-unison-builtin (builtin-Float.asinh x) (asinh x))

(define-unison-builtin (builtin-Float.atan x) (flatan x))

(define-unison-builtin (builtin-Float.atan2 y x) (atan y x))

(define-unison-builtin (builtin-Float.atanh x) (atanh x))

(define-unison-builtin (builtin-Float.cos x) (flcos x))

(define-unison-builtin (builtin-Float.cosh x) (cosh x))

(define-unison-builtin (builtin-Float.fromText t)
  (define mn (string->number (chunked-string->string t)))

  (if mn
    (ref-optional-some mn)
    ref-optional-none))

(define-unison-builtin (builtin-Float.sin x) (flsin x))

(define-unison-builtin (builtin-Float.sinh x) (sinh x))

(define-unison-builtin (builtin-Float.toText x)
  (define base (number->string x))
  (define dotted
    (if (string-contains? base ".")
      base
      (string-replace base "e" ".0e")))

  (string->chunked-string
    (string-replace dotted "+" "")))

(define-unison-builtin (builtin-Float.truncate x)
  (cond
    [(or (= x +inf.0)
         (= x -inf.0)
         (eqv? x +nan.0)
         (eqv? x +nan.f))
     0]
    [else (clamp-integer (inexact->exact (truncate x)))]))

(define-unison-builtin (builtin-Float.logBase base num)
  (log num base))

(define-unison-builtin (builtin-Float.exp n) (exp n))

(define-unison-builtin (builtin-Float.log n) (log n))

(define-unison-builtin (builtin-Float.max n m) (max n m))

(define-unison-builtin (builtin-Float.min n m) (min n m))

(define-unison-builtin (builtin-Float.tan n) (tan n))

(define-unison-builtin (builtin-Float.tanh n) (tanh n))

(define-unison-builtin (builtin-Float.pow n m) (expt n m))

(define-unison-builtin (builtin-Float.sqrt x) (sqrt x))

(define-unison-builtin (builtin-Float.ceiling x)
  (clamp-integer (fl->exact-integer (ceiling x))))

; If someone can suggest a better mechanism for these,
; that would be appreciated.
(define-unison-builtin (builtin-Float.toRepresentation fl)
  (integer-bytes->integer
    (real->floating-point-bytes fl 8 #t) ; big endian
    #f ; unsigned
    #t)) ; big endian

(define-unison-builtin (builtin-Float.fromRepresentation n)
  (floating-point-bytes->real
    (integer->integer-bytes n 8 #f #t) ; unsigned, big endian
    #t)) ; big endian



(define-unison-builtin (builtin-Int.toRepresentation i)
  (integer-bytes->integer
    (integer->integer-bytes i 8 #t #t) ; signed, big endian
    #f #t)) ; unsigned, big endian

(define-unison-builtin (builtin-Int.fromRepresentation n)
  (integer-bytes->integer
    (integer->integer-bytes n 8 #f #t) ; unsigned, big endian
    #t #t)) ; signed, big endian

(define-unison-builtin (builtin-Int.and i j) (bitwise-and i j))

(define-unison-builtin (builtin-Int.complement i)
  (clamp-integer (bitwise-not i)))

(define-unison-builtin (builtin-Int.fromText t)
  (define mn (string->number (chunked-string->string t)))

  (if (and (exact-integer? mn) (>= mn nbit63) (< mn bit63))
    (ref-optional-some mn)
    ref-optional-none))

; more complicated due to negatives
(define-unison-builtin (builtin-Int.leadingZeros i)
  (define len (integer-length i))
  (if (< len 0)
    0
    (- 64 len)))

(define-unison-builtin (builtin-Int.mod i j)
  (clamp-integer (modulo i j)))

(define-unison-builtin (builtin-Int.or i j) (bitwise-ior i j))

(define-unison-builtin (builtin-Int.shiftLeft i k)
  (clamp-integer (arithmetic-shift i k)))

(define-unison-builtin (builtin-Int.shiftRight i k)
  (arithmetic-shift i (- k)))

(define-unison-builtin (builtin-Int.toFloat i) (exact->inexact i))

(define-unison-builtin (builtin-Int.toText i)
  (string->chunked-string (number->string i)))

(define-unison-builtin (builtin-Int.truncate0 i) (if (< i 0) 0 i))

(define-unison-builtin (builtin-Int.xor i j) (bitwise-xor i j))

(define-unison-builtin (builtin-Int.* n m) (clamp-integer (* n m)))

(define-unison-builtin (builtin-Int.pow n m) (clamp-integer (expt n m)))

(define-unison-builtin (builtin-Int.trailingZeros i)
  (define bit (bitwise-first-bit-set i))

  (if (= -1 bit) 64 bit))

; todo: review
(define-unison-builtin (builtin-Int.popCount i)
  (modulo (bitwise-bit-count i) 65))

(define-unison-builtin (builtin-Int.increment i)
  (clamp-integer (add1 i)))

(define-unison-builtin (builtin-Int.negate i)
  (if (> i nbit63) (- i) i))

(define-unison-builtin (builtin-Int.+ i j) (clamp-integer (+ i j)))

(define-unison-builtin (builtin-Int.- i j) (clamp-integer (- i j)))

(define-unison-builtin (builtin-Int./ i j) (floor (/ i j)))

(define-unison-builtin (builtin-Int.signum i) (sgn i))

(define-unison-builtin (builtin-Int.> x y) (> x y))

(define-unison-builtin (builtin-Int.< x y) (< x y))

(define-unison-builtin (builtin-Int.>= x y) (>= x y))

(define-unison-builtin (builtin-Int.<= x y) (<= x y))

(define-unison-builtin (builtin-Int.== x y) (= x y))

(define-unison-builtin (builtin-Int.isEven x) (even? x))

(define-unison-builtin (builtin-Int.isOdd x) (odd? x))



(define-unison-builtin (builtin-Nat.> x y) (> x y))

(define-unison-builtin (builtin-Nat.< x y) (< x y))

(define-unison-builtin (builtin-Nat.>= x y) (>= x y))

(define-unison-builtin (builtin-Nat.<= x y) (<= x y))

(define-unison-builtin (builtin-Nat.== x y) (= x y))

(define-unison-builtin (builtin-Nat.isEven x) (even? x))

(define-unison-builtin (builtin-Nat.isOdd x) (odd? x))

(define-unison-builtin (builtin-Nat.+ m n) (clamp-natural (+ m n)))

(define-unison-builtin (builtin-Nat.drop m n) (natural-max0 (- m n)))

(define-unison-builtin (builtin-Nat.increment n)
  (clamp-natural (add1 n)))

(define-unison-builtin (builtin-Nat.* m n) (clamp-natural (* m n)))

(define-unison-builtin (builtin-Nat./ m n) (quotient m n))

(define-unison-builtin (builtin-Nat.and m n) (bitwise-and m n))

(define-unison-builtin (builtin-Nat.toFloat n) (->fl n))

(define-unison-builtin (builtin-Nat.complement m)
  (wrap-natural (bitwise-not m)))

(define-unison-builtin (builtin-Nat.fromText t)
  (define mn (string->number (chunked-string->string t)))

  (if (and (exact-nonnegative-integer? mn) (< mn bit64))
    (ref-optional-some mn)
    ref-optional-none))

(define-unison-builtin (builtin-Nat.leadingZeros m)
  (- 64 (integer-length m)))

(define-unison-builtin (builtin-Nat.mod m n) (modulo m n))

(define-unison-builtin (builtin-Nat.or m n) (bitwise-ior m n))

(define-unison-builtin (builtin-Nat.pow m n)
  (clamp-natural (expt m n)))

(define-unison-builtin (builtin-Nat.shiftLeft m k)
  (clamp-natural (arithmetic-shift m k)))

(define-unison-builtin (builtin-Nat.shiftRight m k)
  (arithmetic-shift m (- k)))

(define-unison-builtin (builtin-Nat.sub m n)
  (clamp-integer (- m n)))

(define-unison-builtin (builtin-Nat.toInt m)
  ; might need to wrap
  (clamp-integer m))

(define-unison-builtin (builtin-Nat.toText m)
  (string->chunked-string (number->string m)))

(define-unison-builtin (builtin-Nat.xor m n) (bitwise-xor m n))

(define-unison-builtin (builtin-Nat.trailingZeros n)
  (define bit (bitwise-first-bit-set n))

  (if (= -1 bit) 64 bit))

(define-unison-builtin (builtin-Nat.popCount n)
  (bitwise-bit-count n))


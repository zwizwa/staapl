#lang racket/base

;; Threaded Forth for PIC18.

(require
 "tools.rkt"
 "ns.rkt"
 "macro.rkt"
 (only-in "pic18.rkt"
          target-byte-address))

(require/provide
 "asm.rkt"
 "pic18/asm.rkt"
 "pic18/dtc.rkt"
 "pic18/double-math.rkt"
 "coma/macro-forth.rkt"
 "coma/macro-forth-sig.rkt"
 "pic18/dtc-forth-unit.rkt")

(provide
 (all-defined-out)
 target-byte-address)

(define-dasm-collection dasm-collection)

(define/invoke (macro-forth^) (dtc-forth@))

(define (wrap-macro m) m)
(define (wrap-word  m) (macro: ',m _compile))

;; Map primitives.
(define-syntax-rule (define-wrapped wrapper (out in) ...)
  (begin (ns (macro) (define out (wrapper (macro: in)))) ...))

(define-wrapped wrap-macro
  (begin  _begin)
  (again  _again)
  (until  _until)
  (do     _do)
  (while  _while)
  (repeat _repeat)
  (if     _if)
  (else   _else)
  (then   _then)
  )

(define-wrapped wrap-word
  (|;|    _exit)
  (exit   _exit)
  (>r     _>r)
  (r>     _r>)
  (r      _r)
  (rdrop  _rdrop)
  (@      _@)
  (!      _!)
  (ram@   _ram@)
  (ram!   _ram!)
  (rom@   _rom@)
  (dup    _dup)
  (drop   _drop)
  (2drop  _2drop)
  (+      _+)
  (-      _-)
  (*      _*)
  (um*    _um*)
  (invert _invert)
  (negate _negate)
  (not    _not)
  (1+     _1+)
  (1-     _1-)
  (and    _and)
  (or     _or)
  (xor    _xor)
  (lohi   _lohi)
  )



  
  

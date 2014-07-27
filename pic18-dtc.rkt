#lang racket/base

;; Threaded Forth for PIC18.

(require
 "tools.rkt"
 "ns.rkt"
 "macro.rkt"
 (only-in "pic18.rkt"
          target-byte-address))

(require/provide
 "pic18/dtc.rkt"
 "pic18/double-math.rkt"
 "coma/macro-forth.rkt"
 "coma/macro-forth-sig.rkt"
 "pic18/dtc-forth-unit.rkt")

(provide
 (all-defined-out)
 target-byte-address)

(define/invoke (macro-forth^) (dtc-forth@))

(define (wrap-macro m) m)
(define (wrap-word  m) (macro: ',m _compile))

;; Map primitives.
(define-syntax-rule (define-wrapped wrapper (out in) ...)
  (begin (ns (macro) (define out (wrapper (macro: in)))) ...))

(define-wrapped wrap-macro
  (begin _begin)
  (again _again)
  (until _until))

;; Should this snarf all names with underscore prefixes? Otoh, an
;; explicit list is useful for reference.

(define-wrapped wrap-word
  (|;| _exit)
  (>r  _>r)
  (r>  _r>)
  (dup _dup))

  
  

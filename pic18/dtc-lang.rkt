#lang racket/base

(require
 "../tools.rkt"
 "../ns.rkt"
 "../macro.rkt"
 "dtc.rkt"
 "double-math.rkt")

(require/provide
 racket/pretty
 "../coma/macro-forth.rkt"
 "../coma/macro-forth-sig.rkt"
 "dtc-forth-unit.rkt")

(define/invoke (macro-forth^) (dtc-forth@))

;; How to export all identifiers from the macro-forth^ explicitly
;; without doing it manually one at a time, or exporting all
;; identifiers from this module.
;;(provide macro/:) 
(provide
 (all-defined-out)
 ;macro/:
 ;|macro/;|
 ;macro/provide-all
 )

(define-syntax-rule (dtc-module-begin . words)
  (forth-module-begin . words))
(provide
 (rename-out (dtc-module-begin #%module-begin))
 (except-out (all-from-out racket/base) #%module-begin))


(define (wrap-macro m) m)
(define (wrap-word  m) (macro: ',m _compile))

;; Map primitives.
(define-syntax-rule (define-wrapped wrapper (out in) ...)
  (begin (ns (macro) (define out (wrapper (macro: in)))) ...))

(define-wrapped wrap-macro
  (begin _begin)
  (again _again)
  (until _until))

;; Fixme: it's probably better to snarf all names with underscore
;; prefixes from a collection of modules.

(define-wrapped wrap-word
  (|;| _exit)
  (>r  _>r)
  (r>  _r>)
  (dup _dup))

  
  

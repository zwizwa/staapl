#lang racket/base

(require
 "../tools.rkt"
 "../ns.rkt"
 "../macro.rkt"
 "dtc.rkt")

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
 macro/:
 |macro/;|
 macro/provide-all)

(define-syntax-rule (dtc-module-begin . words)
  (forth-module-begin . words))
(provide
 (rename-out (dtc-module-begin #%module-begin))
 (except-out (all-from-out racket/base) #%module-begin))


;; Map primitives.
(ns (macro) (define |;| (macro: ',(macro: _exit) _compile)))


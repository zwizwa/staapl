#lang racket/base

(require "../tools.rkt")

(require
 "../coma/macro-forth-sig.rkt"
 "dtc-forth-unit.rkt")

(define/invoke (macro-forth^) (dtc-forth@))

(define-syntax-rule (dtc-module-begin . words)
  (forth-module-begin
   ;; library "pic18"
   . words))


(provide
 (rename-out (dtc-module-begin #%module-begin))
 (except-out (all-from-out racket/base) #%module-begin))

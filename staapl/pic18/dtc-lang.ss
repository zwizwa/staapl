#lang scheme/base

(require "../tools.ss")

(require
 "../coma/macro-forth-sig.ss"
 "dtc-forth-unit.ss")

(define/invoke (macro-forth^) (dtc-forth@))

(define-syntax-rule (dtc-module-begin . words)
  (forth-module-begin
   ;; library "pic18"
   . words))


(provide
 (rename-out (dtc-module-begin #%module-begin))
 (except-out (all-from-out scheme/base) #%module-begin))

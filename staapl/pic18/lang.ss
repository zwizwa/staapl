#lang scheme/base

;; PLT module language for the pic18 Forth dialect.
;;  = scheme/base
;;  + staapl/pic18
 
(require "../tools.ss")

(provide
 (rename-out (pic18-module-begin #%module-begin))
 (except-out (all-from-out scheme/base) #%module-begin))

(require/provide "../pic18.ss")

;; PIC18 Forth use the normal Forth module compiler with a library
;; path set to find the appropriate .f files.
(define-syntax-rule (pic18-module-begin . words)
  (forth-module-begin library "pic18" . words))







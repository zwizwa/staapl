#lang racket/base

;; PLT module language for the pic18 Forth dialect.
;;  = racket/base
;;  + staapl/pic18
 
(require "../tools.rkt")

(provide
 (rename-out (pic18-module-begin #%module-begin))
 (except-out (all-from-out racket/base) #%module-begin))

(require/provide "pic18.rkt")

;; PIC18 Forth use the normal Forth module compiler with a library
;; path set to find the appropriate .f files.
(define-syntax-rule (pic18-module-begin . words)
  (forth-module-begin library "pic18" . words))







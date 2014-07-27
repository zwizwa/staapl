#lang racket/base

(require
 "../tools.rkt")

(require/provide
 racket/pretty
 "../pic18-dtc.rkt")

(provide
 (all-defined-out))

(define-syntax-rule (dtc-module-begin . words)
  (forth-module-begin . words))
(provide
 (rename-out (dtc-module-begin #%module-begin))
 (except-out (all-from-out racket/base) #%module-begin))



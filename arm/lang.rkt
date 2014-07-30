#lang racket/base

(require
 "../tools.rkt")

(require/provide
 racket/pretty
 "arm.rkt")

(provide
 (all-defined-out))

(define-syntax-rule (module-begin . words)
  (forth-module-begin . words))
(provide
 (rename-out (module-begin #%module-begin))
 (except-out (all-from-out racket/base) #%module-begin))



#lang racket/base

(require
 racket/unit)
(provide
 (all-defined-out)
 (all-from-out racket/unit))


(define-syntax-rule (define/invoke (sig ...) (unit ...))
  (begin
    (define-compound-unit/infer combined@
      (import)
      (export sig ...)
      (link unit ...))
    (define-values/invoke-unit combined@
      (import)
      (export sig ...))))



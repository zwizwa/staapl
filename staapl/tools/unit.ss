#lang scheme/base

(require
 scheme/unit)
(provide
 (all-defined-out)
 (all-from-out scheme/unit))


(define-syntax-rule (define/invoke (sig ...) (unit ...))
  (begin
    (define-compound-unit/infer combined@
      (import)
      (export sig ...)
      (link unit ...))
    (define-values/invoke-unit combined@
      (import)
      (export sig ...))))



#lang scheme/base
(provide (all-defined-out))
(require scheme/pretty)
(define-syntax-rule (** . (expr . args))
  (let ((stx (expr . args)))
    (pretty-print (syntax->datum stx))
    stx))
    
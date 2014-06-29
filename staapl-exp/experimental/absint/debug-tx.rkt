#lang racket/base
(provide (all-defined-out))
(require racket/pretty)
(define-syntax-rule (** . (expr . args))
  (let ((stx (expr . args)))
    (pretty-print (syntax->datum stx))
    stx))
    
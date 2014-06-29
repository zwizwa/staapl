#lang racket/base
(define-syntax-rule (require/provide item ...)
  (begin
    (require item ...)
    (provide (all-from-out item ...))))
(provide require/provide)


#lang racket/base
(require "../sig.rkt")
(provide (all-defined-out))

;; ARM specific wordsets

;; Assembler embedded as macros.
(define-macro-set arm-assembler^
  (add sub addi subi))


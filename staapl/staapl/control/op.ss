#lang scheme/base

(require "../op.ss")

(provide (all-defined-out))

;; Used in virtual compiler and PIC18.

(define-virtual-ops
  (label sym)
  (jw sym)
  (jw/false sym)
  (exit))
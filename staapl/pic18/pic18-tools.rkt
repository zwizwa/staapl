#lang scheme

(provide
 ;; make-target-value-compiler
 tv:)

;; The target-value compiler uses scat: to construct assembler
;; expressions.  (FIXME: this sould later be full parameterization of
;; which assembler to use).

;; FIXME: this was moved from pic18-macro-unit.ss

;; Why?  Probably because it's special-case?

(define-syntax-rule (tv: . code)
  (make-target-value-compiler scat: . code))

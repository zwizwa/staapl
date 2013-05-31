#lang scheme/base
(require "staged-ops.ss")

;; Staged linear equations.

(define-syntax-rule (equalities . x) (void))

(equalities
 (= (A 10))
 (= (B 20))
 (= (C 4))
 (= C (+ (* A x) (* B y))))



     
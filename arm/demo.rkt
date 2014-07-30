#lang racket/base
(require
 "arm.rkt")

;; Test
(define (print-macro macro)
  (state-print-code
   (macro (state:stack))))
(define-syntax-rule (code>  . words) (print-macro (macro: . words)))

(code>
 1 2 3 add
 4 5 6 sub)


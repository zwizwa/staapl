#lang scheme/base

(require "normalform.ss"
         "stx.ss")

;; Simplified constraint language.

(define (test stx)
  (with-handlers
      ((void (lambda (ex) (printf "error: ~a\n~a\n" ex (syntax->datum stx)))))
    (printf "~a\n" (ttx nf stx))))

(test #'(= (+ (* 3 x) (* 2 y y)) 10))
(test #'(< (+ x y) 3))



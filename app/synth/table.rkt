#lang scheme/base
(provide (all-defined-out))

;; See bass->period in control.f
(define notes-24
  (for/list ((n (in-range 24)))
    (inexact->exact
     (floor (/ 61156 (expt 2 (/ n 12)))))))

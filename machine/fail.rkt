#lang racket/base
(provide (all-defined-out))
(define current-fail (make-parameter (lambda (arg) (error 'fail "~a" arg))))
(define (fail . a) ((current-fail) a))
(define (assert x) (unless x (fail)) #t)
(define (asserts e ...) (and (assert e) ...))


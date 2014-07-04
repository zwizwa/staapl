#lang racket/base

(provide sum=0 prod=1 constraints test)

;; Macro frontend for the net/prim compilers.
(require
 (for-syntax racket/base
             "cpl-prim.rkt"
             "cpl-net.rkt"))

(define-syntax sum=0  rule:sum=0)
(define-syntax prod=1 rule:prod=1)
(define-syntax >0     rule:>0)

(define-syntax (constraints stx)
  (syntax-case stx ()
    ((_ in out tmp (rule . formals) ...)
     (let ((inputs   (syntax->list #'in))
           (outputs  (syntax->list #'out))
           (internal (syntax->list #'tmp))
           (rules    (map syntax-local-value (syntax->list #'(rule ...))))
           (params   (map syntax->list (syntax->list #'(formals ...)))))
       (spec->sequence inputs outputs internal rules params)))))

;; TEST
(require racket/pretty)

(define (test)
  (pretty-print
   (syntax->datum
    (expand-once
     #`(constraints
        ;; nodes
        (a b)  ;; ins
        (c d)  ;; outs
        (m)    ;; internal
        ;; rules
        (>0     c)
        (sum=0  a c)
        (prod=1 b d))))))

(test)

;(lambda (a b)
;  (let* ((d (/ 1.0 (* b 1)))
;         (c (- (+ b (+ a 0)))))
;    (values c d)))

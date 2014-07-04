#lang racket/base

(require "../ns-tx.rkt")
(provide (all-defined-out))

;; This module contains compile-time information associated to the 'op
;; datastructure, which is an abstract representation of machine
;; operations that can be used for:

;;   - machine code transformations [OP] -> [OP]
;;   - simulator compilation OP -> (M->M) and assembly OP -> [BIN]

;; The idea is to later use simulation semantics at the syntax level
;; to generate [OP] -> [OP] transformers.

(define-struct op-static (prototype))

(define (op-arity op)
  (length (op-static-prototype op)))


(define (op-check-syntax ins)
  (syntax-case ins ()
    ((rator rand ...)
     (if (not (identifier? #'rator))
         (raise-syntax-error #f "operator is not an identifier" ins #'rator)
         (let* ((id (ns-prefixed #'(op info) #'rator))
                (op (syntax-local-value id (lambda () #f))))
           (unless op
             (raise-syntax-error #f "undefined operator" ins))
           (unless (= (op-arity op)
                      (length (syntax->list #'(rand ...))))
             (raise-syntax-error #f (format "incorrect arity. expected ~a"
                                            (op-static-prototype op)) ins)))))))

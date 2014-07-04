#lang racket/base

;; Core stack operations, used to construct substitution patterns for
;; the partial evaluator, and to map concatenative code to an
;; expression graph.

(define-syntax-rule (stackops definer (name in out) ...)
  (define-syntax-rule (definer macro)
    (begin (macro name in out) ...)))

(stackops for-stackops
 (dup   (a) (a a))
 (drop  (a) ())
 (swap  (a b) (b a))
 (over  (a b) (a b a))
 (swap3 (a b c) (c b a))
 (rot   (a b c) (c a b))
 (-rot  (a b c) (b c a))
 (rot4  (a b c d) (d a b c))
 (-rot4 (a b c d) (b c d a)))

;; NOT ACTUALLY USED

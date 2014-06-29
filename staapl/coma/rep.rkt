#lang racket/base

(require "../scat.rkt")

;; Macros have a slightly different representation over Scat words:
;; primitive macros contain type annotations and composite ones are
;; intensional, so types can be inferred.

(provide (all-defined-out))

(require racket/pretty)
(define (make-primitive-macro fn clauses)
  ;; (pretty-print clauses)
  (make-word fn clauses))


  

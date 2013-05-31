#lang scheme/base

(require "../scat.ss")

;; Macros have a slightly different representation over Scat words:
;; primitive macros contain type annotations and composite ones are
;; intensional, so types can be inferred.

(provide (all-defined-out))

(require scheme/pretty)
(define (make-primitive-macro fn clauses)
  ;; (pretty-print clauses)
  (make-word fn clauses))


  
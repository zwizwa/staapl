#lang racket/base

;; Database query language with pattern matching as a precursor to
;; Prolog & resolution.

;; Given a database of facts, construct a sequence of results in the
;; form of identifier bindings.  In first iteration, use only `?' as a
;; wildcard.  Later, add bindings.

(require "choice.rkt"
         "unify.rkt"
         "fail.rkt"
         "enum.rkt")

;; using `quote' to separate literal symbols from variables
;(define (variable? sym)
;  (eq? #\? (string-ref (symbol->string sym) 0)))

(define facts
  (list->enum '((raining)
                (tomorrow party)
                (tomorrow raining))))




;; (define-syntax (with-pattern p)
  

(define (match-fact s pattern fact)
  (bindings (unify s pattern fact)))

(define (db-query pattern)
  (let ((s (add-free-variables (empty) pattern)))
    (solutions
     (query
      (match-fact s pattern (choice/enum facts))))))


;; (define-syntax-rule (for-pattern pat . body)
;;   ((db-query 'pat)
;;    (lambda (s)
;;      ((pattern-lambda pat . body) s)
;;      #t)))

                            
;; (define-syntax-rule (for/stack-pattern pat . body)
;;   ((db-query 'pat)
;;    (lambda (s stk)
;;      (values #t (cons ((pattern-lambda pat . body) s) stk)))
;;    '()))
     
        
    


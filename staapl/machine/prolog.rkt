#lang scheme/base

(require "choice.ss"
         "unify.ss"
         "database.ss"
         "enum.ss"
         "fail.ss"
         (for-syntax
          scheme/base))


;; Naive variable renaming to prevent name clashes.
(define rename-count (make-parameter #f))
(define (rename-rule rule)
  (define (bump-count!)
    (rename-count (add1 (rename-count))))                      
  (define (rename expr)
    (map-variables
     (lambda (var)
       (string->symbol (format "~a~a" var (rename-count))))
     expr))
  (bump-count!)
  (map rename rule))



(define-syntax-rule (make-rules rule ...)
  (list->enum (list 'rule ...)))
(define rule-head car)
(define rule-body cdr)


(define (unify-rule store pattern rule)
  (let* ((rule (rename-rule rule))
         (store (add-free-variables store (rule-head rule))))
    (for/fold
        ((store (unify store pattern (rule-head rule))))
        ((bpat (rule-body rule)))
      (unify-pattern/db store bpat))))

(define rules-db (make-parameter #f))
(define (unify-pattern/db store pattern)
  (unify-rule store pattern (choice/enum (rules-db))))

(define (solve pattern)
  (solutions
   (query/parameterize ((rename-count 0))
    (bindings
     (unify-pattern/db
      (add-free-variables (empty) pattern)
      pattern)))))

(rules-db
 (make-rules
  ((summer))
  ((green ?x)
   ;; <=
   (tree ?x)
   (summer))
  ((tree pine))
  ((green algae))))


#lang racket/base

(require mzlib/match
         "choice.rkt"
         "unify.rkt"
         "database.rkt"
         "enum.rkt"
         "fail.rkt"
         (for-syntax
          racket/base
          "unify.rkt"))

;; FIXME: this doesn't extend frames when necessary.

;; Compiling the rules to an expression makes it possible to use
;; Scheme's binding mechanism to implement local rule names.  Note
;; that the head still needs unification: the `match-lambda' is there
;; just to perform substitution of rule variables, not the logical
;; variables propagated through the query.
(define-syntax (rule-lambda stx)
 (define (map-id stx var sym)
   (let map ((stx stx))
     (syntax-case stx ()
       ((a . d) #`(#,(map #'a) . #,(map #'d)))
       (() #'())
       (x (if (variable? (syntax->datum #'x))
              (var #'x)
              (sym #'x))))))
 (define (id x) x)
 (syntax-case stx ()
   ((_ (head . body))
    #`(match-lambda
       (#,(map-id #'head id (lambda (x) #'_)) ;; dont'care
        `#,(map-id #'(head . body) (lambda (x) #`,#,x) id))
       (else (fail))))))

;; Rules are implemented as a function mapping a pattern to head+body
;; with rule's local variables substituted.
(define-syntax-rule (make-rules rule ...)
  (list->enum (list (rule-lambda rule) ...)))

;; Unification performs the following steps:
;;  1. substitute rule variables
;;  2. unify store with head
;;  3. recursively unify body
(define (unify-rule store pattern rule-subst)
  (match (rule-subst pattern) ;; 1.
         ((head . body)
          (for/fold ((store (unify store pattern head))) ;; 2.
                    ((bpat body))
            (unify-pattern/db store bpat))))) ;; 3.

(define rules-db (make-parameter #f))
(define (unify-pattern/db store pattern)
  (unify-rule store pattern (choice/enum (rules-db))))

(define (solve pattern)
  (solutions
   (query
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


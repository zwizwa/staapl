#lang racket/base
;; Syntax transformers for statespace DSL.

(require
 racket/dict
 racket/match
 racket/pretty
 (for-template
  racket/base))
(provide
 eqs-flat-lambda
 eqs-vector-lambda
 parse-eqs-sets)

;; Tools.
(define (false . _) #f)
(define (bool x) (if x #t #f))

;; Use mutable hashes throughout.  All other ops use the mutable
;; dictionary interface.
(define dict make-hash)

(define (keys d) (dict-map d (lambda (k v) k)))

;; Dictionary set operations: ops are defined on keys, and values
;; are dragged along.  (FIXME: reuse library functions?)
(define (dict-op logic-op a b)
  (let ((r (dict)))
    (for (((k v) (in-dict a)))
      (when (logic-op (dict-ref b k false))
        (dict-set! r k v)))
    r))
(define (dict-and    a b) (dict-op bool a b))
(define (dict-andnot a b) (dict-op not a b))

;; Identifier sets.  The set implementation uses a mutable
;; dictionary.  Index is on symbol, i.e. identifier equality is
;; implemented as symbol equality.  Identifier is kept value to keep
;; track of lexical context.
(define (id-add! d id)
  (dict-set! d (syntax->datum id) id))

;; Recurse over expression, collect ids.
(define (id-add-rec! d top-expr)
  (let rec! ((expr top-expr))
    (syntax-case expr ()
      ((op . rands)  (for ((a (syntax->list #'rands))) (rec! a)))
      (node          (id-add! d #'node)))))

;; Perform "register allocation": assign a number to each symbol.
(define (dict-regalloc! dict)
  (for (((k v) (in-dict dict))
        (n (in-naturals)))
    (dict-set! dict k n))
  dict)

(define-struct eqs-sets (form eqs in out state))

(define (eqs-print eqs)
  (define (line name dict)
    (printf "~a:" name)
    (for (((k v) (in-dict dict)))
      (printf " ~s" k))
    (newline))
  (match eqs
         ((struct eqs-sets (form eqs in out state))
          (begin
            (line "in"    in)
            (line "out"   out)
            (line "state" state)
            (pretty-print (syntax->datum form))))))

  
      
    

  
;; Construct sets from equation spec.
(define (parse-eqs-sets form)
  ;; Collect RHS and LHS ids + keep track of var->eq map.
  (let ((eqs   (dict))
        (lhs   (dict))
        (rhs   (dict)))
    (for ((eq (syntax->list form)))
      (syntax-case eq ()
        ((var expr)
         (begin
           (dict-set!   eqs (syntax->datum #'var) #'expr)
           (id-add!     lhs #'var)
           (id-add-rec! rhs #'expr)))))
    ;; Separate into state/in/out classes.
    (let* ((state (dict-regalloc! (dict-and    lhs rhs)))
           (in    (dict-regalloc! (dict-andnot rhs state)))
           (out   (dict-regalloc! (dict-andnot lhs state))))
      (make-eqs-sets form eqs
                     in out state))))

(define (eqs-flat-lambda sets)
  (eqs-print sets)
  (match sets
         ((struct eqs-sets (form eqs in out state))
          (let* ((id (lambda (sym) (datum->syntax form sym))) ;; restore context
                 (gather-eqs
                  ;; Retrieve equations associated to tags in dict
                  (lambda (dict)
                    (for/list (((k v) (in-dict dict)))
                      #`(#,(id k) #,(dict-ref eqs k))))))
                        
            #`(lambda (#,@(map id (keys state))
                       #,@(map id (keys in)))
                (let (#,@(gather-eqs state)
                      #,@(gather-eqs out))
                  (values #,@(map id (keys state))
                          #,@(map id (keys out)))))))))
    
(define (eqs-vector-lambda sets)
  (eqs-print sets)
  (match sets
         ((struct eqs-sets (form eqs in out state))
          (let* ((id
                  (lambda (sym) (datum->syntax form sym))) ;; restore context
                 (vrefs
                  (lambda (vec dict)
                    (for/list (((sym index) (in-dict dict)))
                      #`(#,(id sym) (vector-ref #,vec #,index)))))
                 (gather-eqs
                  ;; Retrieve equations associated to tags in dict
                  (lambda (dict)
                    (for/list (((k v) (in-dict dict)))
                      #`(#,(id k) #,(dict-ref eqs k))))))
                        
            #`(lambda (vstate vin)
                (let (#,@(vrefs #'vstate state)
                      #,@(vrefs #'vin in))
                  (let (#,@(gather-eqs state)
                        #,@(gather-eqs out))
                    (values (vector #,@(map id (keys state)))
                            (vector #,@(map id (keys out)))))))))))
    



     

#lang racket/base
(require (for-syntax racket/base) "stx.ss"
         srfi/1)

;; Produce normal forms for several types of expressions using
;; algebraic equalities as directed rewrite rules.

(provide (all-defined-out))

;; These syntactic operations transform the input clauses into a form
;; the constraint system can handle (declaration of propagator
;; instances).


(define (minus stx)
  (syntax-case stx (-)
    ((- x) #'x)
    (x #'(- x))))



;; Reduce multiplications (including negation).
(define (r/mul stx)
  ;; If the result is a sum, we can recurse on the operands.
  (define (sum a b) #`(+ #,(r/mul a) #,(r/mul b)))
  (define sum? (op? +))

  ;; Expanding product arguments might expose sums, in which case the
  ;; whole expression needs to re-expand.  Since every step performs
  ;; one multiplicative reduction, the loop will terminate.
  (define (prod . args)
    (let ((args (map r/mul args)))
      (let ((stx #`(* #,@args)))
        (if (ormap sum? args)
            (r/mul stx) stx))))

  (syntax-case stx (* - +)
    ((* (+ a b) c) (sum #'(* a b) #'(* b c)))
    ((* a (+ b c)) (sum #'(* a b) #'(* a c)))
    ((* a b)       (prod #'a #'b))
    ((+ a b)       (sum #'a #'b))
    ((- a b)       (sum #'a #'(- b)))
    ((- a)         (prod #'-1 #'a))
    (_ stx)))

;; Convert to nested unary/binary operations
(define (u/b stx)
  (define (ub op . args) #`(#,op #,@(map u/b args)))
  (syntax-case stx ()
    ((op a)       (ub #'op #'a))
    ((op a b)     (ub #'op #'a #'b))
    ((op a b ...) (ub #'op #'a #'(op b ...)))
    (a #'a)
    ))

;; FIXME: flatten * and + after reduction.

(define (flatten op? [sub (lambda (x) x)])
  (lambda (stx)
    (let flatten_ ((stx stx))
      (if (op? stx)
          (syntax-case stx ()
            ((op a b) (append (flatten_ #'a) (flatten_ #'b))))
          (list (sub stx))))))

;; Sum-of-products
(define (flatten/* stx) #`(* #,@((flatten (op? *)) stx)))
(define (flatten/+ stx) #`(+ #,@((flatten (op? +) flatten/*) stx)))

;; Count symbolic terms.
(define (identifier-cons stx lst)
  (if (identifier? stx) (cons stx lst) lst))
(define (term-variables stx)
  (syntax-case stx (*)
    ((* . factors)
     (foldl identifier-cons '() (syntax->list #'factors)))))
(define (term-variable-lset stx)
  (apply lset-union bound-identifier=?
         (map list (term-variables stx))))

(define (term-order stx) (length (term-variables stx)))

(define (sort-order sum-stx)
  (syntax-case sum-stx (+)
    ((+ . terms)
     #`(+ #,@(sort
              (syntax->list #'terms)
              > #:key term-order)))))

;; Return a sum of products expression, with terms in the sum sorted
;; according to nb variables in the product.
(define (sop stx)
  (sort-order (flatten/+ (r/mul (u/b stx)))))

;; Convert binary expression to normal form (comparison wrt. zero)
(define (nf stx)
  (syntax-case stx (= < > <= >= +)
    ((< a b)  (nf #'(> b a)))
    ((<= a b) (nf #'(>= b a)))
    ((= 0 a)  (nf #'(= a 0)))
    ((= a 0)  #`(constr:= #,(sop #'a)))
    ((> a 0)  #`(constr:> #,(sop #'a)))
    ((>= a 0) #`(constr:>= #,(sop #'a)))
    ((op a b) (nf #'(op (- a b) 0)))
    ))

;(define-syntax (normalform stx)
;  (syntax-case stx ()
;    ((_ form) (nf #'form))))


;; Convert linear forms to matrix representation.  First gather all
;; variables, then filter them out of the terms.


    
(define (forms->matrix_FIXME lst)
  (define variables
    (for/fold ((vars '())) ((stx lst))
              (syntax-case stx (+)
                ((+ . terms)
                 (for/fold ((vars vars)) ((term (syntax->list #'terms)))
                           (lset-union
                            bound-identifier=?
                            vars (term-variable-lset term)))))))
  ;; not implemented
  variables)
  

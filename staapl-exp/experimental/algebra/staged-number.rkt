#lang racket/unit
(require "ring-sig.ss"
         "vec-sig.ss"
         "staged-ops.ss"
         "combinators.ss"
         racket/match)

(import vec^)
(export ring^)



(define (with-rep fn)
  (lambda args
    (parameterize ((current-identifier= bound-identifier=?)
                   (current-string->identifier string->identifier)
                   (current-number= =))
      (apply fn args))))

(define zero (make-number 0))
(define one  (make-number 1))

(define add
  (with-rep
   (make-staged-binop
    #:communitative #t
    #:eval     +     ;; evaluate expression
    #:postpone #'+   ;; generate expression
    #:unit?    zero?)))

(define mul
  (with-rep
   (make-staged-binop
    #:communitative #t
    #:eval     *
    #:postpone #'*
    #:unit?    (lambda (x) (= 1 x))
    #:->null   (lambda (x) (and (zero? x) zero)))))


(define (sum-list lst)
  (let-values (((numbers variables) (numbers/variables lst)))
    ((split-op zero add)
     (cons (foldl add zero numbers) variables))))


(define field? #t)

;; FIXME: define staged properly
(define 2-norm (make-staged-op
                #:eval (lambda (n)
                         (if (>= n 0) n (- n)))
                #:postpone #'2-norm))

(define neg (make-staged-op
             #:eval -
             #:postpone #'-))
(define inv (make-staged-op
             #:eval (lambda (n) (/ 1 n))
             #:postpone #'inv))


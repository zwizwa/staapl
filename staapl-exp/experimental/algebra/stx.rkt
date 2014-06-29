#lang racket/base
(provide (all-defined-out))
(define-syntax-rule (op? op)
  (lambda (stx)
    (syntax-case stx (op)
      ((op . _) #t) (_ #f))))

(define (ttx tx . args) 
  (syntax->datum
   (syntax-case (apply tx args) () (x #'x))))

(define (->sym stx)
  (let ((dat (syntax->datum stx)))
    (and (symbol? dat) dat)))

(define (map-stx fn)
  (lambda (stx) (map fn (syntax->list stx))))

(require racket/dict)
(define (make-stx-hash)
  (make-custom-hash
   (lambda (a b)
     (bound-identifier=? a b))

   ;; Stx with the same symbolic structure will end up in the same
   ;; hash bins.  I don't know how to code them more uniformly.
   (lambda (id) (equal-hash-code (syntax->datum id)))
   (lambda (id) (equal-secondary-hash-code (syntax->datum id)))))


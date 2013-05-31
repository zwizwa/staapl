#lang scheme/base
(require (lib "match.ss"))

;; The core of a scheme evaluator with applicative order
;; evaluation, without primitives.

(define (m-eval expr env)
  (match expr
         ;; abstraction
         (('lambda formals body)
          (list 'closure formals body env))
         ;; application
         ((fn . args)
          (m-apply (m-eval fn  env)
                   (map (lambda (x) (m-eval x env)) args)
                   env))
         ;; variable reference
         ((? symbol?)
          (let ((binding (assoc expr env)))
            (unless binding
              (error 'undefined-variable "~a" expr))
            (cdr binding)))
         ;; literal
         (else expr)))
  
(define (m-apply fn args env)
  ;; fn is a lambda form
  (match fn
         (('closure formals body env)
          (m-eval body
                  (append
                   (map cons formals args)
                   env)))
         (else
          (error 'apply-error "~a ~a" fn args))))


;; To make it meta-circular instead of pretty, implement: match map
;; assoc append error, in terms of the primitives: if car cdr pair?
;; symbol?

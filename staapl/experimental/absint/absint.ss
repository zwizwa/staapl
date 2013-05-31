#lang scheme

;; The interfaces.  Basic language syntax and a single-function module.
(define-signature num^ (add sub mul lit))
(define-signature fun^ (fun))

;; The language interpretations.
(define-unit num-eval@
  (import)
  (export num^)
  (define add +)
  (define sub -)
  (define mul *)
  (define (lit x) x))


(define-unit num-gen@
  (import)
  (export num^)
  (define (add a b) `(add ,a ,b))
  (define (sub a b) `(sub ,a ,b))
  (define (mul a b) `(mul ,a ,b))
  (define (lit x) `(lit ,x)))


;; A test program
(define-unit fun@
  (import num^)
  (export fun^)
  (define (fun x y)
    (let ((a (add x y))
          (b (sub x y)))
      (mul a b))))

;; Test program linked to interpretations.
(define-compound-unit/infer fun-eval@
  (import)   
  (export fun^ num^) 
  (link fun@ num-eval@))

(define-compound-unit/infer fun-gen@
  (import)
  (export fun^ num^)
  (link fun@ num-gen@))


(define-syntax-rule (run unit@ num@)
  ((lambda ()
     (define-values/invoke-unit/infer
       (link unit@ num@))
     fun)))


(define-syntax-rule (lambda/num formals . body)
  (unit
    (import num^)
    (export)
    (lambda formals . body)))

    
  
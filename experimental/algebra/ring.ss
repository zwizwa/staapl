#lang scheme/base
(require

 ;; interfaces
 "ring-sig.ss"
 "vec-sig.ss"

 ;; implementations
 "staged-number.ss"
 "vec-list.ss"
 

 ;; tools
 "../tools.ss"


 ;; debug
 "staged-ops.ss"

 )


(define/invoke (ring^ vec^) (staged-number@ vec-list@))

;; Convert symbolic <-> packed representation.
(define (tree-map fn)
  (lambda (x)
    (let sub ((x x))
      (cond
       ((null? x) x)
       ((pair? x) (cons (sub (car x)) (sub (cdr x))))
       (else (fn x))))))
(define pack
  (tree-map
   (lambda (x)
     (if (symbol? x)
         (make-variable (symbol->identifier x))
         (make-number x)))))
(define unpack
  (tree-map
   (lambda (x)
     (cond
      ((variable? x) (syntax->datum (variable-id x)))
      ((number? x) (number-value x))
      (else (error 'unpack-unknown "~a" x))))))

;; Convert from memoized form back to nested expression.  (This uses
;; the `bindings' parameter.)
(define (un-memoize expr)
  (tree-map
   (lambda (x)
     (cond
      ((variable? expr)
       (let ((e (variable->expr expr)))
         (if e (un-memoize e) expr)))
      (else expr)))))

      
  

(current-pack pack)
(current-unpack unpack)

(define (matrix sym) (list->mat (pack sym)))

(define a (matrix '((a 0) (0 b))))
(define b (matrix '((b 0) (d a))))

(require scheme/pretty)
(define (print-mat m)
  (pretty-print (unpack (mat->list m))))


(define m
  (matrix '(( 2 -1  0)
            (-1  2 -1)
            ( 0 -1  2))))

;; CT predicates are used for compile-time decisions: they don't have
;; a run-time equivalent.
(define (ctp:> a b)
  (let ((na (->number a))
        (nb (->number b)))
    (or (and a b) (error 'ct:>))
    (> na nb)))

(define (gauss-jordan m)
  (mat-gauss-jordan 2-norm ctp:> m))

(define (mat-inv m)
  (gauss-jordan (mat-cat-columns m (mat-one (mat-nb-rows m)))))
  
;; (define m1 (mat-inv m))

;; (print-mat m1)
;; (print-mat (mat-mul m1 m))


;(emit (pack '(a 10)))
;(emit (pack '(b 10)))
;(emit (pack '(c 12)))

(define eq1
  (matrix '(( 2 -1  0  a)
            (-1  2 -1  b)
            ( 0 -1  2  c))))

(print-mat (gauss-jordan eq1))



; (parameterize ((bindings '()))
  (emit (pack '(X 123)))
  (unpack (add (pack 'X) (pack 'X)))
; )

(require (for-syntax scheme/base))

(define-syntax (id=? stx)
  (syntax-case stx ()
    ((_ a b)
     #`(quote #,(bound-identifier=? #'a #'b)))))

  

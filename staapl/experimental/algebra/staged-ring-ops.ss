#lang scheme/base

;; Code emission / variable creation for abstract interpretation of
;; algebraic expressions.

(require scheme/match)
(provide (all-defined-out))



;; Variables need equality and generation from string.  By default
;; variables are represented as symbols.

(define current-identifier= (make-parameter eq?))
(define current-string->identifier (make-parameter string->symbol))
(define current-number= (make-parameter =))


;; This code only distinguishes numbers from values, and doesn't touch
;; contents.

(define-struct variable (id))
(define-struct number (value))

(define (->number x)
  (and (number? x) (number-value x)))

;; Comparison.
(define (make= type? unpack param-type=)
  (lambda (a b)
    (and (type? a)
         (type? b)
         ((param-type=) (unpack a) (unpack b)))))
(define variable= (make= variable? variable-id current-identifier=))
(define number=   (make= number? number-value current-number=))
(define (ob= ob1 ob2)
  (or (variable= ob1 ob2)
      (number= ob1 ob2)))

;; Name generation.
(define tmp-count (make-parameter 0))
(define (make-temp)
  (let ((n (tmp-count)))
    (tmp-count (add1 n))
    (make-variable
     ((current-string->identifier) (format "v~a" n)))))


;; Code emission.
;; Statement is a list: (var (op . args)) where var is client's
;; identifier and args is a list of identifiers / numbers.
(define (staged-unpack x)
  (cond
   ((pair? x) (cons (staged-unpack (car x)) (staged-unpack (cdr x))))
   ((variable? x) (variable-id x))
   ((number? x) (number-value x))
   (else x)))
(define code (make-parameter '()))

(define (print-expr st)
  (printf ";; ~a\n" (staged-unpack st)))

(define (emit st)
  (code (cons st (code)))
  (print-expr st))


  
;; Memoization for common subexpression elimination.
(define (expr= t1 t2)
  (or (and (pair? t1)
           (pair? t2)
           (expr= (car t1) (car t2))
           (expr= (cdr t1) (cdr t2)))
      (and (null? t1) (null? t2) #t)
      (ob= t1 t2)))

(define statements (make-parameter '()))
(define (register s)
  (statements (cons s (statements))))
(define (expr->variable expr)
  (ormap (match-lambda
          ((list var expr_)
           (and (expr= expr expr_) var)))
         (statements)))
(define (variable->expr var)
  (ormap (match-lambda
          ((list var_ expr)
           (and (variable= var var_) expr)))
         (statements)))
  

(define (make-expression expr)
  (let* ((tmp (make-temp))
         (st (list tmp expr)))
    (register st)
    (emit st)
    tmp))

;; Communitative binary op.
(define (staged-postpone-binop comm fn a b)
  (let ((expr (cons fn (list a b)))
        (expr/swap (and comm (cons fn (list b a)))))
    (or (expr->variable expr)
        (expr->variable expr/swap)
        (make-expression expr))))

;; Binary op interpretation
(define (make-staged-binop #:eval eval
                           #:postpone [postpone #f]
                           #:communitative [comm #f]
                           #:unit? [unit? #f]
                           #:->null [->null (lambda (x) #f)])
  (lambda (x y)
    (define (make-code)
      (unless postpone (error 'postpone))
      (staged-postpone-binop comm
       (make-variable postpone) x y))
    (define (number-op x/y)
      (lambda (n)
        (if (and unit? (unit? n)) x/y
            (or (->null n) (make-code)))))
    (cond
     ((let ((nx (->number x))
            (ny (->number y)))
        (and nx ny (make-number (eval nx ny)))))
     ((->number x) => (number-op y))
     ((->number y) => (number-op x))
     (else (make-code)))))


;; Separate numbers from variables (I.e. for fixing summation order).
(define (numbers/variables lst)
  (values (filter number? lst)
          (filter (compose not number?) lst)))

(define (staged-postpone-op fn x)
  (let ((expr (list fn x)))
    (or (expr->variable expr)
        (make-expression expr))))

(define (make-staged-op #:eval eval
                        #:postpone postpone)
  (lambda (x)
    (let ((n (->number x)))
      (if n
          (make-number (eval n))
          (begin
            (unless postpone (error 'postpone))
            (staged-postpone-op (make-variable postpone) x))))))


;; Convert from ANF back to nested expression.
(define (un-anf expr)
  (cond
   ((pair? expr)
    (cons (un-anf (car expr)) (un-anf (cdr expr))))
   ((null? expr) '())
   ((variable? expr)
    (let ((e (variable->expr expr)))
      (if e (un-anf e) expr)))
   (else expr)))

      
  
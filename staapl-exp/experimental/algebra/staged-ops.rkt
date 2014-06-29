#lang racket/base

;; Staging of algebraic expressions.

;; The algorithm attempts to evaluate expessions at compile time.  If
;; this fails (when a particular operation refers to a variable) the
;; operation is appended to a sequence of SSA assignment statements.

;; This sequence can be interpreted as a name <-> value binding
;; environment (a `let*' form), or as a description of a data flow
;; graph (in which case it is a representative of an equivalence class
;; of `let*' forms).

(require racket/match
         racket/contract)

;; (provide (all-defined-out))

(provide
 current-identifier=
 current-string->identifier
 current-number=
 current-unpack
 current-pack
 bindings
 variable?
 number?
 emit
 make-staged-op
 make-staged-binop
 )

;; This code only distinguishes numbers from values, and doesn't touch
;; contents.

(define-struct variable (id))
(define-struct number (value))

(define (abstract-value? x)
  (or (variable? x) (number? x)))

(provide/contract
 [symbol->identifier (-> symbol? identifier?)]
 [string->identifier (-> string? identifier?)]
 [make-variable (-> any/c variable?)]  [variable-id (-> variable? any/c)]
 [make-number (-> any/c number?)]      [number-value (-> number? any/c)]
 [->number (-> abstract-value? (or/c #f any/c))]
 [numbers/variables (-> (listof abstract-value?)
                        (values (listof number?)
                                (listof variable?)))]
 [variable->expr (-> variable? (or/c #f any/c))]
 [expr->variable (-> any/c (or/c #f variable?))]

 )
 

;; If an operation cannot be postponed until run-time, the following
;; thunk is called.  Useful for mapping to `fail' in a backtracking
;; search.
(define current-postpone-error
  (make-parameter (lambda () (error 'postpone-error))))

;; Variables need equality and generation from string.  By default we
;; use symbols to represent variables.
(define current-identifier= (make-parameter eq?))
(define current-string->identifier (make-parameter string->symbol))
(define current-number= (make-parameter =))
(define current-unpack (make-parameter (lambda (x) x)))
(define current-pack (make-parameter (lambda (x) x)))

;; For embedding in hygienic macros, variables need to be identifiers.
(define (symbol->identifier x) (datum->syntax #f x))
(define (string->identifier x) (symbol->identifier (string->symbol x)))



;; Attempt to convert a number|variable to a numeric value for compile
;; time evaluation (constant folding).  This will query the current
;; environment (constant propagation).
(define (->number x)
  (match x
         ((struct number (val)) val)
         ((struct variable (name))
          (let ((expr (variable->expr x)))
            (match expr
                   ((struct number (val)) val)
                   (else #f))))))

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
     ((current-string->identifier) (format "r~a" n)))))


(define bindings (make-parameter '()))

(define (print-expr x)
  (printf ";; ~a\n" ((current-unpack) x)))

(define (emit statement)
  (bindings (cons statement (bindings)))
  (print-expr statement))

  
;; The emitted code can can be used in two directions: as a binding
;; table for variable reference to implement constant propagation, and
;; as memoization table for common subexpression elimination.
(define (expr= t1 t2)
  (or (and (pair? t1)
           (pair? t2)
           (expr= (car t1) (car t2))
           (expr= (cdr t1) (cdr t2)))
      (and (null? t1) (null? t2) #t)
      (ob= t1 t2)))

(define (expr->variable expr [env (bindings)])
  (ormap (match-lambda
          ((list var expr_)
           (and (expr= expr expr_) var)))
         env))
(define (variable->expr var [env (bindings)])
  (ormap (match-lambda
          ((list var_ expr)
           (and (variable= var var_) expr)))
         env))

(define (make-statement expr)
  (let* ((tmp (make-temp))
         (st (list tmp expr)))
    (emit st)
    tmp))

;; Communitative binary op.
(define (staged-postpone-binop comm fn a b)
  (let ((expr (cons fn (list a b)))
        (expr/swap (and comm (cons fn (list b a)))))
    (or (expr->variable expr)
        (expr->variable expr/swap)
        (make-statement expr))))

;; Binary op interpretation
(define (make-staged-binop #:eval eval
                           #:postpone [postpone #f]
                           #:communitative [comm #f]
                           #:unit? [unit? #f]
                           #:->null [->null (lambda (x) #f)])
  (lambda (x y)
    (define (make-code)
      (unless postpone ((current-postpone-error)))
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
        (make-statement expr))))

(define (make-staged-op #:eval eval
                        #:postpone [postpone #f])
  (lambda (x)
    (let ((n (->number x)))
      (if n
          (make-number (eval n))
          (begin
            (unless postpone ((current-postpone-error)))
            (staged-postpone-op (make-variable postpone) x))))))



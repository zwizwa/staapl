#lang racket/base

;; Unfication on s-expressions using a single assignment store.
;; See CTM[1] section 2.8.2 p.98-

;; [1] http://www.info.ucl.ac.be/~pvr/book.html

;; Unification is the symmetric generalization of pattern matching,
;; where both sides of the equation may contain variables.

(provide add-free-variables
         map-variables
         bindings
         binding-ref
         unify
         empty
         variable?

         )

(require racket/match "fail.rkt"
         (for-syntax racket/base))
;; The store is a collection of variable bindings, where each variable
;; can be bound to a partial value.

(define-struct store (equivs bindings))
;; `equivs'   is a set of sets of var, a set of equivalence set of variables
;; `bindings' is a set of (var . value) pairs, a set of determined variables
(define (empty) (make-store '() '()))


(define (bindings s)
  (match s ((struct store (e bs))
            (if (null? e) bs (fail)))))

;; Simple-minded list implementation.
(define make-set list)
(define set-union append)
(define (set-remove set . els)
  (let loop ((set set)
             (els els))
    (if (null? els) set
        (loop (remove (car els) set) (cdr els)))))
(define set-map map)

(define (set-member? set el) (member el set))
(define-syntax-rule (set-ormap . a) (ormap . a))

;; Determined variables
(define (variable? x)
  (and (symbol? x)
       (not (eq? x '?))
       (eq? #\? (string-ref (symbol->string x) 0))))

(define-struct binding (var val))
(define bind make-binding)
(define (binding-ref set ref-var)
  (if (null? set) #f
      (match (car set)
             ((struct binding (store-var val))
              (if (eq? ref-var store-var) val
                  (binding-ref (cdr set) ref-var))))))


;; Binding
(define (store-join-es s es1 es2)
  (match s
         ((struct store (equivs bindings))
          (make-store
           (set-union
            (make-set (set-union es1 es2))
            (set-remove equivs es1 es2))
           bindings))))

(define (store-bind s es1 data)
  (match s
         ((struct store (equivs bindings))
          (make-store
           (set-remove equivs es1)
           (set-union 
            (set-map (lambda (var)
                       (bind var data))
                     es1)
            bindings)))))

(define (store-var-es s var)
  (set-ormap (lambda (set)
               (and (set-member? set var) set))
             (store-equivs s)))
(define (store-var-ref s var)
  (binding-ref (store-bindings s) var))

(define (store-defined? s var)
  (or (store-var-es s var)
      (store-var-ref s var)))

(define (store-declare s . vars)
  (match s ((struct store (es det))
            (make-store (set-union
                         (map (lambda (var)
                                (when (store-defined? s var)
                                  (error 'defined "~s" var))
                                (make-set var))
                              vars)
                         es) det))))
                                 

;; Unify nested s-expressions.  See CTM 2.8.2.2 p.101
(define (make-unify-error x1 x2)
  (lambda (y1 y2)
    (error 'contradiction "~a=~a -> ~a=~a"
           x1 x2 y1 y2)))



(define (varref/value s var/val)
  (if (variable? var/val)
      (let ((es  (store-var-es s  var/val))
            (det (store-var-ref s var/val)))
        (unless (or es det) (error 'undefined "~a" var/val))
        (values es det))
      (values #f var/val)))

(define-syntax-rule (do/ state (fn arg ...) ...)
  (let ((s state))
    (let* ((s (fn s arg ...)) ...) s)))

(define (wildcard? x)
  (eq? x '?))

(define (unify s x1 x2)
  (define did (make-set))
  ;; (printf "~a=~a\n" x1 x2)
  (let recurse ((x1 x1) (x2 x2) (s s))
    (let ((this (list x1 x2))
          (same (lambda (x1 x2) (if (equal? x1 x2) s (fail x1 x2)))))
      (if (set-member? did this) s
          (begin
            (set! did (set-union (make-set this) did))
            (let-values
                (((es1 d1) (varref/value s x1))
                 ((es2 d2) (varref/value s x2)))
              (cond
               ((or (wildcard? x1) (wildcard? x2)) s)
               ((and es1 (eq? es1 es2) s))
               ((and es1 es2) (store-join-es s es1 es2))
               ((and es1 d2)  (store-bind s es1 d2))
               ((and es2 d1)  (store-bind s es2 d1))
               ((and (list? d1) (list? d2))
                (unless (= (length d1) (length d2)) (fail))
                (foldl recurse s d1 d2))
               (else (same d1 d2)))))))))

(define (swap fn) (lambda (x y) (fn y x)))


(define (add-free-variables s expr)
  (cond
   ((null? expr) s)
   ((variable? expr) (store-declare s expr))
   ((pair? expr) (add-free-variables
                  (add-free-variables s (car expr))
                  (cdr expr)))
   (else s)))

(define (map-variables fn expr)
  (cond
   ((null? expr) expr)
   ((variable? expr) (fn expr))
   ((pair? expr) (cons (map-variables fn (car expr))
                       (map-variables fn (cdr expr))))
   (else expr)))


  

;; Lexical binding form for value stores from pattern syntax.
(define (deref s lst)
  (apply values (map
                 (lambda (v)
                   (binding-ref s v))
                 lst)))






;; (define-syntax (pattern-lambda stx)
;;   (define (free-vars stx lst)
;;     (syntax-case stx ()
;;       ((a . d) (free-vars #'a (free-vars #'b lst)))
;;       (x (if (variable? (syntax->datum #'x))
;;              (cons #'x lst)
;;              lst))))
;;   (syntax-case stx ()
;;     ((_ pat . body)
;;      (syntax-case (reverse (free-vars #'pat '())) ()
;;        ((var ...)
;;         #`(lambda (s)
;;             (let-values (((var ...)
;;                           (deref s '(var ...))))
;;               . body)))))))
 

;; TEST

;; (define s
;;   (do/ (empty)
;;        (declare a b c foo)
;;        (unify   a (foo 'bar))
;;        (unify   a (123 'bar))
;;        ))


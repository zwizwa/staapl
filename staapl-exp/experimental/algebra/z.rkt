#lang racket/base
(require "stx.ss"
         "normalform.ss")
(require racket/dict)
(provide (all-defined-out))

;; Normal form for algebraic expressions with a delay operator.
;; Propagate all occurances of `z' to the parameters.


;; ** ASSOCIATIVITY **

(define (is-assoc-id? stx)
  (case (syntax->datum stx)
    ((+ * max min) #t)
    (else #f)))

(define id-eq? bound-identifier=?)

;; Propagate binary op transformation.
(define (map-args fn stx)
  (syntax-case stx ()
    ((op . as) #`(op #,@((map-stx fn) #'as)))
    (a         #'a)))

;; Rotate right binary tree of associative ops leaving unary ops alone.
(define (rotate stx [assoc? is-assoc-id?])
  (let rotate ((stx stx))
    (syntax-case stx ()
      ((op1 (op2 a b) c)
       (and (id-eq? #'op1 #'op2) (assoc? #'op1))
       (rotate #`(op1 a (op1 b c))))
      (else (map-args rotate stx)))))


;; Flatten associative operators in a rotated tree.
(define (rr->flat stx [assoc? is-assoc-id?])
  (let flatten ((stx stx))
    (syntax-case stx ()
      ((op1 a (op2 b c))
       (and (id-eq? #'op1 #'op2) (assoc? #'op1))
       (with-syntax ((a_ (flatten #'a)))
         (syntax-case    (flatten #'(op1 b c)) ()
           ((_ . as) #`(op1 a_ . as)))))
      (else (map-args flatten stx)))))

;; Reverse
(define (flat->rr stx [assoc? is-assoc-id?])
  (let rr ((stx stx))
    (syntax-case stx ()
      ((op a b c . ds)
       (and (assoc? #'op))
       (with-syntax ((a_  (rr #'a))
                     (op_ (rr #'(op b c . ds))))
       #`(op a_ op_)))
      (else (map-args rr stx)))))


;; ** SHIFT **

;; Propagate delay operator to variable references as ~ operator.
;; Syntax needs to be unary/binary.

(define (r/z stx)
  
  (define sub (map-stx r/z))
  (define (sub/z n)
    (map-stx (lambda (stx) (r/z #`((z #,n) #,stx)))))

  (define (num stx)
    (let ((n (syntax->datum stx)))
      (unless (number? n)
        (raise-syntax-error stx))
      n))

  (define (nsym n a)
    (if (identifier? a)
        (datum->syntax a
                       (string->symbol
                        (format "~a~a"
                                (syntax->datum a)
                                (num n))))
        (raise-syntax-error stx)))

  (syntax-case stx (z)
    ((z a)             (r/z #`((z 1) a)))
    (((z n) (z a))     (r/z #`((z #,(add1 (num #'n))) a)))
    (((z n) (op . as)) #`(op #,@((sub/z #'n) #'as)))

    (((z n) a)         #`(~ a n))
    ((op . as)         #`(op #,@(sub #'as)))
    (a                 #'(~ a 0))
    ))


;; DATAFLOW NETWORKS

;; A dfl data structure is represented by a dictionary.  The key is a
;; stream reference while the expression can be any nested arithmetic
;; expression.
(define (stx->dfl stx)
  (for/list ((n/e (syntax->list stx)))
    (syntax-case n/e ()
      ((node expr) (list #'node #'expr)))))
(define (dfl-map key-fn expr-fn dfl)
  (map (lambda (node)
         (list (key-fn (car node)) (expr-fn (cadr node))))
       dfl))


;; The tilde-form of an expression is an intermediate dataflow form
;; where each signal is represented as (~ <name> <offset> ...).  This
;; will be translated to a loop in imperative Scheme (mappable to C).

;; Straight conversion to imperative Scheme.

(define (tx-tilde-expr stx [loopvar #'i])
  (let tx-expr ((stx stx))
    (syntax-case stx (~)
      ((~ var offset) #`(ref var (- #,loopvar offset)))
      ((op . args) #`(op #,@((map-stx tx-expr) #'args)))
      (a #'a))))
(define (dfl->scheme dfl [loopvar #'i] [endx #'n])
  (define (tx-expr stx)
    (syntax-case stx (~)
      ((~ var offset) #`(ref var (+ #,loopvar offset)))
      ((op . args) #`(op #,@((map-stx tx-tilde-expr) #'args)))
      (a #'a)))
  #`(for ((#,loopvar (in-range #,endx)))
      #,@(for/list ((n/e (dfl-map tx-expr tx-expr dfl)))
           #`(set! #,@n/e))))
    
(define (dfl/z->scheme stx)
  (dfl->scheme
   (dfl-map r/z r/z (stx->dfl stx))))


;; Same, with more elaborate memoization.


;; To generate pre/post load/store code, uniquely named lexical
;; variables are constructed from the delay operators, together with a
;; dictionary that maps them to the originals.
(define (tilde-vars stx)
  (define d (make-stx-hash))
  (let find! ((stx stx))
    (syntax-case stx (~)
      ((~ id n)
       (let ((now (syntax->datum #'n))
             (max (dict-ref d #'id (lambda () #f))))
         (when (or (not max)
                   (< max now))
           (dict-set! d #'id now))))
      ((op . args)
       ((map-stx find!) #'args))))
  d)
;; Piggy-back on previous for dfl collection.
(define (dfl-tilde-vars dfl)
  (tilde-vars (datum->syntax #f (cons 'dummy-op (apply append dfl)))))

(define (ref->var stx)
  (datum->syntax
   #f (string->symbol
       (apply format "~a_~a" (cdr (syntax->datum stx))))))

(define (vi->ref a n) #`(~ #,a #,n))




;; Memoized version of dfl/z->scheme

;; dereference pointers into local variables + rotate.

;; FIXME: this has several problems, but the general idea seems ok.
;; Specify in/out and factor it out a bit.

(define (tilde->memo stx)
  (syntax-case stx (~)
    ((~ id n) (ref->var stx))
    (else (map-args tilde->memo stx))))

(define (loop-body dfl)
  (let* ((d (dfl-tilde-vars dfl))  ;; var -> nb.echos
         ;; Delay line prefill.
         (pre
          (for*/list (((k v) (in-dict d))
                      (i (in-range v)))
            (let ((ref (vi->ref k (add1 i))))                     
              #`(set! #,(ref->var ref)
                      #,(tx-tilde-expr ref)))))

         ;; In-loop load
         (load
          (for/list (((k v) (in-dict d)))
            (let ((iref (vi->ref k #'0))
                  (var  (ref->var (vi->ref k 0))))
              #`(set! #,var
                      #,(tx-tilde-expr iref)))))

         ;; In-loop store
         (store
          (for/list (((k v) (in-dict d)))
            (let ((iref (vi->ref k #'0))
                  (var  (ref->var (vi->ref k 0))))
              #`(set! 
                      #,(tx-tilde-expr iref)
                      #,var))))

         ;; In-loop shift
         (shift
          (reverse
           (for*/list (((k v) (in-dict d))
                       (i (in-range v)))
             (with-syntax ((v- (ref->var (vi->ref k i)))
                           (v+ (ref->var (vi->ref k (add1 i)))))
               #`(set! v+ v-))))))

    #`(begin
        (begin #,@pre)
        (for ((i (in-range n)))
          #,@load
          #,@(for/list ((n/e (dfl-map tilde->memo tilde->memo dfl)))
               #`(set! #,@n/e))
          #,@store
          #,@shift))))



#lang scheme/base
(require scheme/promise scheme/control)
(provide (all-defined-out))

;; The code in this file connects traversal functions (map), generator
;; thunks, sequences, lazy lists and zippers.

;; Definitions:
;;
;;   * traversal function: higher order function that visits all
;;     elements of a data structure in sequence and possibly
;;     constructs an updated version.  (map: update, for-each: simply
;;     visit)
;;
;;   * generator: a thunk with internal state, which generates a new
;;     value every time it is invoked.  it is associated to a
;;     predicate done? that can distinguish the end-of-sequence value.
;;   
;;   * acor: a function parameterized by the "produce" and "end"
;;     continuations (asymmetric coroutine).
;;
;;   * sequence: plt scheme's abstraction used in sequence
;;     comprehensions (for)
;;
;;   * lazy list: list constructors (cons x y) | '() wrapped in a
;;     promise.
;;
;;   * zipper: a two-way generator = a traversal function (map) turned
;;     inside-out (a coroutine).


;; *** LAZY LISTS ***

;; The base representation used is the lazy list.  Generators,
;; acors, sequences and (read-only) traversal functions are
;; related to this base form.

;; Convert generator to lazy list
(define (generator->lazy-list g done?)
  (let next ()
    (delay
      (let ((item (g)))
        (if (done? item) '()
            (cons item (next)))))))

(define gen->ll generator->lazy-list)


(define (values* x) (apply values x))
(define (pass . args) (values* args))

;; Convert lazy list to sequence.
(define (in-lazy-list ll [output pass])
  (make-do-sequence
   (lambda ()
     (define (ll-car x)   (output (car (force x))))
     (define (ll-cdr x)   (cdr (force x)))
     (define (ll-more? x) (pair? (force x)))
     (values ll-car ll-cdr ll ll-more? void void))))

(define in-ll in-lazy-list)

;; Composition.
(define (sequence->list seq) (for/list ((el seq)) el))
(define seq->l sequence->list)

(define (generator->list gen done?) (seq->l (in-gen gen done?)))
(define gen->l generator->list)

(define (in-gen gen done?) (in-ll (gen->ll gen done?)))


;; map -> find
(define (find mapper el/false collection)
  (prompt
   (mapper
    (lambda args
      (let ((el (apply el/false args)))
        (if el (abort el) #f)))
    collection)
   #f))


;; Macros for converting a sequential program -> lazy list.  ll-begin
;; marks the generating context and produces the empty list when it
;; terminates, and ll-produce generates the next list element.

(define-syntax-rule (ll-begin . body)
  (reset (begin . body) (delay '())))
(define-syntax-rule (ll-produce x)
  (shift k (delay (cons x (k #f)))))
(define (ll-end) (abort (delay '())))

;; Wraps these macros in a HOF that converts a generator body
;; parameterized by 'produce and 'end into a lazy list.

;; The 'produce continuation by default only takes one value.

(define (ll-acor task-body [pack values*])
  (define (produce . xs) (ll-produce (pack xs)))
  (ll-begin
   (if (eq? 1 (procedure-arity task-body))
       (task-body produce)
       (task-body produce ll-end))))

;; Direct bridge between sequences and acors, able to transfer
;; multiple values through 'produce.
(define (in-acor task-body [unpack values*])
  (in-ll (ll-acor task-body pass) ;; pack values in list
         unpack))

;; LL from sequence.  This also accepts lists, strings and vectors.
;; Due to multiple values this needs apply.
(define (sequence->lazy-list seq [pack values])
  (let-values (((more? next) (sequence-generate seq)))
    (ll-begin
     (let loop ()
       (when (more?)
         (ll-produce (call-with-values next pack))
         (loop))))))

(define seq->ll sequence->lazy-list)
(define (seq->list-ll seq) (seq->ll seq list))



;; These can be used to transform any collection with associated map
;; function into a lazy list.  A lazy list is like a zipper, but it's
;; uni-directional and memoized (It is derived from for-each, not
;; map).

(define (map->ll map collection)
  (ll-begin
   (map (lambda (el) (ll-produce el))  ;; return value is not used!
        collection)))




;; Sequence from map.
(define (in-map map collection)
  (in-ll (map->ll map collection)))

;; List from lazy list
(define (lazy-list->list ll) (seq->l (in-ll ll)))
(define ll->l lazy-list->list)

;; comprehension to lazy list
(define-syntax-rule (for/lazy-list clauses . body)
  (ll-begin (for clauses (ll-produce (begin . body)))))
(define-syntax-rule (for/ll . a) (for/lazy-list . a))

;; Map over lazy list
(define (ll-map fn ll) (for/ll ((e (in-ll ll))) (fn e)))
          
  


(define (ll-uncons x [make-fake-tail (lambda () (error 'll-uncons-null))])
  (let ((p (force x)))
    (if (null? p)
        (ll-uncons (make-fake-tail))
        (values (car p) (cdr p)))))

;; Bind a number of elements of a lazy list.
(define-syntax ll-let
  (syntax-rules ()
    ((_ (rest) expr . body)
     (let ((rest expr)) . body))
    ((_ (e es ...) expr . body)
     (let-values (((e tail) (ll-uncons expr)))
       (ll-let (es ...) tail . body)))))
          

;; For convenience, an (infinite) fake tail can be appended to the
;; list to make sure the function succeeds.
(define (ll-take in-n in-ll [make-fake-tail (lambda () (error 'll-take-underflow))])
  (let next ((n in-n)
             (ll in-ll)
             (acc '()))
    (if (<= n 0)
        (values (reverse acc) ll)
        (let-values (((e ll+) (ll-uncons ll make-fake-tail)))
          (next (sub1 n) ll+ (cons e acc))))))
  
(define (in-append . seqs)
  (in-ll
   (apply ll-append
          (map seq->list-ll seqs)) ;; pack values as list
   values*)) ;; unpack list as values

(define (ll-append ll . lls)
;;   (printf "lls: ~a\n" lls)
  (delay
    (let ((l (force ll)))
      (if (pair? l)
          (let ((e (car l))
                (ll+ (cdr l)))
            (cons e (apply ll-append ll+ lls)))
          (if (null? lls)
              '()
              (force (apply ll-append lls)))))))



;; ZIPPER


;; A zipper is to map what a lazy list is to for-each.

;; A zipper is represented by a data structure containing the current
;; element and a continuation which takes the replacement of that
;; element.  This continuation returns either a new zipper or the
;; final data structure.

(define-struct zipper (element yield)) ;; fixme: sum type!
(define (map->zipper collection [map map-list])
  (reset
   (map (lambda (el)
          (shift k (make-zipper el k)))
        collection)))

(define (map-list fn lst)
  (map (lambda (el) (or (fn el) el)) lst))


;; A file descriptor interface. See:
;; http://okmij.org/ftp/papers/context-OS.pdf

;; "open" could be polymorphic based on type of c which associates a
;; default traversal function.
(define zipper-open map->zipper)

(define (zipper-read z)    (zipper-element z))
(define (zipper-write z x) ((zipper-yield z) x))

;; The proper way to do this is to not use "in-band" data (#f) to
;; signal absence of update, but let the continuation accept a Maybe
;; type.

(define (zipper-close z)
  (let next ((z z))
    (if (zipper? z)
        (next (zipper-write z #f))
        z)))

            
              
  
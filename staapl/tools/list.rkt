#lang racket/base

(require
 racket/match
 "base.rkt"
 (for-syntax
  racket/base))


(provide
 (all-defined-out))

(define first  car)
(define second cadr)
(define third  caddr)
(define (uncons x) (values (car x) (cdr x)))

(define (take n lst)
  (if (or (zero? n)
          (null? lst))
      '()
      (cons (car lst)
            (take (sub1 n) (cdr lst)))))

(define (diff-lists l1 l2)
  (if (and (not (null? l1))
           (not (null? l2))
           (eq? (car l1) (car l2)))
      (diff-lists (cdr l1) (cdr l2))
      (values l1 l2)))

(define (port->list port [read read])
  (let next ((lst '()))
    (let ((atom (read port)))
      (if (eof-object? atom)
          (reverse lst)
          (next (cons atom lst))))))


(define (hash-add-alist! hash alist)
  (for-each
   (lambda (x)
     (hash-set! hash (car x) (cdr x)))
   alist)
  hash)

(define (alist->hash alist)
  (hash-add-alist! (make-hash) alist))
(define (hash->alist hash)
  (hash-map hash cons))


(define-syntax-rule (values->list . body)
  (call-with-values
      (lambda () . body)
    list))

(define (dip fn)
  (lambda (lst)
    (cons (car lst)
          (fn (cdr lst)))))

(define (splash fn)
  (lambda (args)
    (apply fn args)))

(define (for-each* fn . lsts)
  (apply for-each
         (lambda (args)
           (apply fn args))
         lsts))

(define (map* fn . lsts)
  (apply map
         (lambda (x)
           (apply fn x))
         lsts))

;; Transform a list like
;; ((a . 1) (a . 2) (a . 3) (b . 4) (b . 5)) into
;; ((a . (1 2 3)) (b . (4 5)))

;; Order is preserved.

;; On streams, this is more of a 'dispatch' operation..

(define (collect eq? lst)
  (let ((tags (list->lset eq? (map car lst))))
    (map
     (lambda (current-tag)
       (cons current-tag
             (foldr
              (match-lambda* ((list (list-rest tag data) collection)
                              (if (eq? tag current-tag)
                                  (cons data collection)
                                  collection)))
              '() lst)))
     tags)))

;; (list->lset = '(3 1 1 2 2 2 3 3 2))
(define (list->lset same? lst)
  (foldr
   (lambda (head rest)
     (if (contains same? head rest)
         rest
         (cons head rest)))
   '() lst))

;; not using 'member' but following the lset api with plugin comparison
(define (contains same? el lst)
  (memf (lambda (e) (same? el e)) lst))

(define-syntax-rule (with-stack push! . expr)
  (let* ((stack '())
         (push! (lambda (x)
                  (set! stack (cons x stack)))))
    (begin . expr)
    stack))

(define-syntax-rule (push! stack value)
  (let ((rest stack))
    (set! stack (cons value rest))))

(define-syntax-rule (pop! stack)
  (if (null? stack)
      (error 'stack-underflow
             "pop!: no elements on: ~a" 'stack)
      (let ((top (car stack)))
        (set! stack (cdr stack))
        top)))


;; Structure types.

(define-syntax (struct-match stx)
  (define (type stx)
    (syntax-case stx ()
      ((tag . fields)
       #`(struct tag #,(map type (syntax->list #'fields))))
      (name
       (identifier? #'name)
       #`name)))
  (syntax-case stx ()
    ((_ in (pattern template) ...)
     #`(match in
              #,(map (lambda (p t)
                       #`(#,(type p) #,t))
                     (syntax->list #'(pattern ...))
                     (syntax->list #'(template ...)))))))

(define-syntax struct-match-lambda
  (syntax-rules ()
    ((_ . body)
     (lambda (x) (struct-match x . body)))))




;; Stack of stacks for parsing into list of lists.


(define (make-sos n)
  (for/list ((i (in-range n))) '()))


;; In most cases the result will be an 'open' sos, so collapse
;; it. Note that collapsing is not idempotent!

(define (sos->list sos)
  (reverse
   (list-ref
    (sos-collapse sos)
    (- (length sos) 1))))
    
(define (sos-push sos x)
  (cons (cons x (car sos))
        (cdr sos)))

(define (sos-collapse sos [n (- (length sos) 1)])
  (if (zero? n)
      sos
      (cons '()
            (sos-collapse
             (sos-push (cdr sos)
                       (reverse (car sos)))
             (- n 1)))))

;; Save an entry to the 2nd stack leaving the 1st stack intact.  The
;; entry is in the order that will eventually end up after sos->list.
(define (sos-stash d entry)
  (match d ((list-rest 1st 2nd more)
            (list* 1st (cons entry 2nd) more))))
             

;; Imperative interface.
(define-syntax-rule (sos-push! sos x)
  (begin
    (set! sos (sos-push sos x))
    sos))

(define-syntax-rule (sos-collapse! sos n)
  (begin
    (set! sos (sos-collapse sos n))
    sos))

;; The most common use is to parse a flat list into a lists of lists.
;; Extension: process multiple lists at once.
(define (split-list head? combine . ls)
  (define sos (make-sos 2))
  (let next ((ls ls))
    (if (null? (car ls))
        (sos->list sos)
        (let ((els (map car ls)))
          (when (apply head? els)
            (sos-collapse! sos 1))
          (sos-push! sos (apply combine els))
          (next (map cdr ls))))))
          
          


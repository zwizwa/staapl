#lang scheme/base

(provide split-op split-half split-first)

;; Using associativity to construct different dataflow dependencies
;; for fold/reduce.

(define (split-half lst)
  (let ((l (length lst)))
    (when (< l 2) (error 'split-half "~a" lst))
    (let ((half (round (/ l 2)))
          (tail #f))
      (values
       (let loop ((i 0) (lst lst))
         (if (< i half)
             (cons (car lst) (loop (add1 i) (cdr lst)))
             (begin (set! tail lst) '())))
       tail))))

(define (split-first lst)
  (values (list (car lst)) (cdr lst)))

(define (split-op zero op [split split-half])
  (lambda (lst)
    (let subop ((lst lst))
      (case (length lst)
        ((0) zero)
        ((1) (car lst))
        ((2) (apply op lst))
        (else
         (let-values (((left right) (split lst)))
           (op (subop left)
               (subop right))))))))


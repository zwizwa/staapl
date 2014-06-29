#lang racket/base
;; Small shared utilities.
(require
 "../tools.rkt")
(provide
 (all-defined-out)
 (all-from-out "../tools.rkt"))

(define (->int x) (inexact->exact (floor x)))
(define (equal-head? a b)
  (or (null? a)
      (and (equal? (car a) (car b))
           (equal-head? (cdr a) (cdr b)))))


(define (b->w lst) (join-nibble-list lst 0 8))
(define (fixedpoint x [scale 1.0] [b 16]) (* scale (/ x (<<< 1 b))))

(define (collect-size size list-gen-thunk)
  (let more ((s size)
             (d '()))
    (if (<= s 0)
        (apply append (reverse d))
        (let* ((data (list-gen-thunk))
               (len  (length data)))
          (more (- s len) (cons data d))))))



(define (split els lst)
  (let next ((e els)
             (l lst)
             (r '()))
    (if (or
         (null? l)
         (zero? e))
        (values (reverse r) l)
        (next (sub1 e) (cdr l) (cons (car l) r)))))

(define (distribute-size size distribute-list lst)
  (let next ((l lst))
    (let-values (((head tail) (split size l)))
      (distribute-list head)
      (unless (null? tail)
        (next tail)))))



(define (pad short-lst need-bytes)
  (let ((n (- need-bytes (length short-lst))))
    (append short-lst
            (build-list n (lambda _ #xFF)))))  ;; FIXME



;; Interactive tools
(define (dump thunk)
  (for ((p (in-hex-printer 0 4 2 8))
        (d (thunk)))
    (p d))
  (newline))

(define (dump-list lst)
  (for ((p (in-hex-printer 0 4 2 8))
        (d lst))
       (p d))
  (newline))
  

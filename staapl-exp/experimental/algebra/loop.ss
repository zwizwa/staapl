#lang scheme/base

;; Algebra of loop transformations.

;; Phase 1: build a substrate that makes verification of laws simple.
;; Let's take circular vectors of exact numbers.

(define N 10)
(define max-e 100)


(define (random-element . _)
  (inexact->exact (floor (* max-e (random)))))
(define (random-stream . _)
  (build-list N random-element))
(define (stream= s1 s2)
  (andmap = s1 s2))

(define stream-length length)
(define stream-ref list-ref)

(define (I stream) stream)
(define (Z stream)
  (append (cdr stream) (list (car stream))))


(define s1 (random-stream))
(define s2 (random-stream))


(define (quickcheck fn1 fn2 n [times 10])
  (for ((i (in-range times)))
    (let ((streams (build-list n random-stream)))
      (let ((r1 (apply fn1 streams))
            (r2 (apply fn2 streams)))
        (unless (stream= r1 r2)
          (printf "ERROR: in ~a\n~a != ~a\n" streams r1 r2))))))



(define (loop_I_Z binop s)
  (let-values
      (((rout last)
        (for/fold
            ((rout '())
             (ze (car s)))
            ((e (cdr s)))
            (values (cons (binop e ze) rout) e))))
    (reverse (cons (binop (car s) last) rout))))



;(define (_loop_I_Z binop s)
;  (let ((n (stream-length s)))
;    (let loop ((i 1)
;               (ze (stream-ref s 0))
               
                 
          
    
(quickcheck
 (lambda (s) (map + (I s) (Z s)))
 (lambda (s) (loop_I_Z + s))
 1 10)


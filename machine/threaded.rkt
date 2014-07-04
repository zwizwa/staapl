#lang racket/base
(require racket/control)
(provide make-counter state)
(define state (make-parameter #f))

(define (suspend/param param)
  (let ((state+value
         (let ((state (param)))
           (shift k
             (lambda (value)
               (k (cons state value)))))))
    (param (car state+value))
    (cdr state+value)))

(define (make-counter)
  (reset
   (parameterize ((state 0))
     (let loop ()
       (printf "state = ~s\n" (state))
       (let ((hole (suspend/param state)))
         (printf "hole = ~s\n" hole))
       (state (add1 (state)))
       (loop)))))

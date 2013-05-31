#lang scheme/base

(require "statespace.ss")


(define-model (foo
               (s1 s2)  ;; state
               (i1 i2)  ;; input
               (o1 o2)) ;; output
  (model-update
   ((s1 (+ s1 s2))
    (s2 (- s1 s2))
    (o1 (* s1 i1))
    (o2 (* s2 i2)))))




;; (lambda (vstate vin)
;;   (let ((x1 (vector-ref vin 0))
;;         (x2 (vector-ref vin 1))
;;         (s  (vector-ref vstate 0)))
;;     (let ((y (+ x1 x2)))
;;       (let (s+ (* y y))
;;         (values (vector s+)
;;                 (vector s+))))))
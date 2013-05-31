#lang scheme/base


(provide (all-defined-out))
(require scheme/unit)

(define-signature ring^
  (add  neg  zero
   mul  inv  one

   2-norm
   
   sum-list  ;; for staging, this uses optimized data flow / control flow
   field?    ;; i.e. for communitative multi

   ))


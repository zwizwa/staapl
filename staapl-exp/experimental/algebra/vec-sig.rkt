#lang racket/base

;; Matrix/vector representation

(provide (all-defined-out))
(require racket/unit)

(define-signature vec^
  (;; matrix algebra can represent linear algebra. 
   list->mat  ;; list of rows -> arbitrary, but this makes printing simpler
   mat->list
   mat? transpose
   mat-map
   mat-nb-rows
   mat-nb-columns
   mat-cat-rows
   mat-cat-columns
   mat-one mat-zero
   
   ;; matrix ops
   mat-add mat-mul
   mat-gauss-jordan

   ;; vec <-> mat conversions
   rows columns vec->column vec->row

   ;; FP representation of linear algebra (scalars, vectors,
   ;; functionals, linear transformations)
   list->vec vec? vec->list vec-map vec-dim
   inner-product
   ))


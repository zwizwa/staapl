#lang scheme/unit
(require "vec-sig.ss"
         "ring-sig.ss"

         ;; debug
         ;; "staged-ring-ops.ss"

         )
(import ring^)
(export vec^)

;; Simple list representation.

;; This is used in staged operations where matrix structure is
;; eliminated in the final program, so efficiency is not important.

;; Matrices
(define-struct mat_ (lst))
(define list->mat make-mat_)
(define mat->list mat_-lst)
(define mat? mat_?)
(define (transpose m) (list->mat (apply map list (mat->list m))))


;; Matrix algebra.
(define (mat-map fn . ms)
  (let ((ls (map mat->list ms)))
    (list->mat
     (apply map (lambda rs (apply map fn rs)) ls)))) 

(define (mat-add a b) (mat-map add a b))

(define (mat-mul a b)
  (let ((a-rows (mat->list a))
        (b-columns (mat->list (transpose b))))
    (list->mat
     (for/list ((r a-rows))
       (for/list ((c b-columns))
         (sum-list (map mul r c)))))))


;; Perform Gauss-Jordan elimination
;; http://en.wikipedia.org/wiki/Gauss%E2%80%93Jordan_elimination
(define (mat-gauss-jordan abs > m)
  ;; Pivoting: sort rows according to absolute value.
  (define (ll-pivot ll)
    (sort ll > #:key
          (lambda (row) (abs (car row)))))
  
  ;; Eliminate one column.  The result is implicitly prefixed with a
  ;; column: [0 ... 0 1 0 ... 0], where the 1 is the pivot row.
  (define (ll-elim-column top-rows pivot-row bottom-rows)
    (let ((inv-p (inv (car pivot-row))))
      (define (/p x) (mul x inv-p))  ;; pivot row scaling factor
      (let ((1-row (map /p (cdr pivot-row))))
        (define (->zero row)
          (let ((head (car row)))
            (let ((_h (neg head)))
              (map (lambda (a b)
                     (add a (mul b _h)))
                   (cdr row)
                   1-row))))
        (values
         (map ->zero top-rows) 1-row
         (map ->zero bottom-rows)))))
  ;; Recursively eliminate.  The resulting matrix is implicitly
  ;; prefixed with the unit matrix, i.e. if a matrix was originally
  ;; postfixed with unit, the result is the inverse.
  (define (ll-elim-all ll)
    (let next ((r-top '())
               (bottom ll))
      (if (null? bottom) (reverse r-top)
          (let ((bottom/p (ll-pivot bottom)))
            (let-values (((t p b)
                          (ll-elim-column r-top
                                          (car bottom/p)
                                          (cdr bottom/p))))
              (next (cons p t) b))))))
                            
  ;; wrap/unwrap
  (list->mat (ll-elim-all (mat->list m))))


(define-syntax-rule (for/mat ((i j) (n m)) . body)
  (list->mat
   (for/list ((i (in-range 0 n)))
     (for/list ((j (in-range 0 m)))
       . body))))
(define (number->mat num)
  (lambda (n)
    (for/mat ((i j) (n n))
      (if (= i j) num zero))))
(define (list->mat-diag lst)
  (let ((n (length lst)))
    (for/mat ((i j) (n n))
      (if (= i j)
          (list-ref lst i)
          zero))))
(define mat-one (number->mat one))
(define mat-zero (number->mat zero))
  
(define (mat-cat-rows . ms)
  (list->mat (apply append (map mat->list ms))))
(define (mat-cat-columns . ms)
  (transpose (apply mat-cat-rows (map transpose ms))))

(define (mat-nb-rows m) (length (mat->list m)))
(define (mat-nb-columns m) (length (car (mat->list m))))
  
;; Linear algebra (scalars, vectors, functionals, linear
;; transformations) written in terms of matrix algebra.

;; type: vec
(define-struct vec_ (lst))
(define list->vec make-vec_)
(define (vec-dim vec) (length (vec_-lst vec)))
(define (vec-map fn . vs) (list->vec (apply map fn (map vec->list vs))))
(define vec? vec_?)
(define vec->list vec_-lst)

;; conversion: vec <-> mat
(define (vec->row vec) (list->mat (list (vec->list vec))))
(define (vec->column vec) (transpose (vec->row vec)))
(define (rows m) (map list->vector (mat->list m)))
(define (columns m) (rows (transpose m)))

;; vec combinators
(define (inner-product v1 v2)
  (caar (mat->list (mat-mul (vec->row v1) (vec->column v2)))))

    
#lang scheme/base

;; IP - image processing compiler

;; This file contains code to expand a grid expression to a single
;; nested fully specified C code loop. It doesn't perform any
;; highlevel code transformations

;; #'(+ (a 0 0) (a 1 1)) ->
;;
;; {
;;   int i;
;;   for (i = 0; i < (400 * 300); i += 300)
;;   {
;;     float* a_p1 = a + (i + (1 * 300));
;;     float* a_p0 = a + (i + (0 * 300));
;;     float* x_p0 = x + (i + (0 * 300));
;;     {
;;       int j;
;;       for (j = 0; j < 300; j += 1)
;;       {
;;         float* a_p1_p1 = a_p1 + (j + 1);
;;         float* a_p0_p0 = a_p0 + (j + 0);
;;         float* x_p0_p0 = x_p0 + (j + 0);
;;         *(x_p0_p0) = (*(a_p0_p0) + *(a_p1_p1));
;;       }
;;     }
;;   }
;; }

;; Originally written in march 2007. See http://zwizwa.be/ramblings/staapl/20070330-160157

;; This code operates on possibly unwrapped syntax objects. All
;; matching happens using syntax-case, so it's immune to this, but if
;; the output should be a syntax object, wrap it in datum->syntax or
;; quote-syntax.

;; The language used here consists of nested expressions:
;; (operator . args)
;; (grid . index)
;; constant


(require "../tools.ss")
(require "../poke/cgen.ss")


;; PARSING

;; Instead of using an abstract data type for representing abstract
;; syntax trees, I'm using this tree-map function which applies
;; transformations to 3 different syntactic elements:
;; grid/operator/constant.

;; IP programs consist of opertors and grids. currently, operators are
;; fixed (primitive), but later they can come from a different name
;; space.

(define (ip-operator? x)
  (case (->sexp x)
    ((+ - / * abs) #t)      ;; primitive math ops
    ((= statements let) #t) ;; intermediate rep keywords
    (else #f)))

;; FIXME: use lexical scope to determine which are grid names, not the
;; other way around.

(define (make-src-iterator _cons _map)
  (lambda (fn-grid
           fn-operator
           fn-constant
           src)
    (let down ((s src))
      (syntax-case s ()
        ;; operator
        ((op . args)
         (ip-operator? #'op)
         (_cons (fn-operator #'op)
                (_map down
                      (syntax->list #'args))))
        ;; grid
        ((name . index)
         (fn-grid #'(name . index)))

        ;; constants
        (c (fn-constant #'c))))))

;; Map (pure) and for-each (impure))
(define src-map (make-src-iterator cons map))
(define src-for (make-src-iterator void for-each))

;; Map over grid applications.
(define (grid-map fn src) (src-map fn id id src))


;; get a list of grid dimensions from a source text
(define (src->grids src-stx)
  (define src (syntax->datum src-stx))

  (let ((grids '())) ;; linear accumulator

    (define (register! grid/index-stx)
      (let ((grid/index (->sexp grid/index-stx)))
        (let ((grid/dims ((dip length) grid/index))
              (already (assoc (car grid/index) grids)))
        
          (if (not already)
              (push! grids grid/dims)
              (when (not (equal? already grid/dims))
                (error 'invalid-dimensions
                       "new: ~a, was: ~a"
                       grid/dims
                       already))))))

    (src-for register! void void src)
    grids))



;; Compute output dimension.
(define (grids->dim grids)
  (apply max (map cdr grids)))

(define (src->dim src)
  (grids->dim (src->grids src))) 



;; Parse the source code:
;; - tag grid expressions
;; - pad dimensions
;; - tag loop body
  
(define (parse-grid-language src dim/range)
  (list 'loop  ;; Loop body starts at outer nesting level.
        (let ((maxdim (length dim/range)))
          (grid-map
           (transformer
            ((name . indices)
             ;; #`(grid name . indices)
             #`(grid name #,@(pad-indices #'indices maxdim))
             ))
           src))))




;; C CODE GENERATION

;; This operates in 3 steps:
;;  * Transform the grid language syntax from above to explicitly tagged expressions.
;;  * Perform recursive tree unfolding for all loop bodies over tree dimensions.
;;  * Define base behaviour for expanded 'grid' and 'loop' tags.

;; TOOLS

(define mul
  (transformer
   (()  #'1)
   ((a) #'a)
   ((a . r) #`(* a #,(mul #'r)))))

(define (num=? _x _n)
  (let ((x (->sexp _x))
        (n (->sexp _n)))
    (and (number? x)
         (= x n))))

(define (pad-indices index-stx dim)
  (let loop ((index (syntax->list index-stx)))
    (if (< (length index) dim)
        (loop (cons 0 index))
        index)))

;; Synthesize variable name for computed pointer.
(define (offset->name base offset-stx)
  (define offset (syntax->datum offset-stx))
  (datum->syntax base
                 (string->symbol
                  (format "~s_~s~s"
                          (syntax->datum base)
                          (if (< offset 0) 'm 'p)
                          (abs offset)))))

;; Generate for loop block, introducing local bindings.
(define (for-loop dim/range bindings body)
  (let ((loopvar (caar dim/range))
        (range (map cdr dim/range)))
    
    #`(block
       (var int #,loopvar)
       (for-head (=  #,loopvar 0)
                 (<  #,loopvar #,(mul range))
                 (+= #,loopvar #,(mul (cdr range))))
       (block
        (vars #,@bindings)
        #,body))))

;; Pointer initialization at start of for loop.
(define (pointer-init dim/range a offset)
  (let ((range (map cdr dim/range)))
    #`(+ #,a
         (+ #,(caar dim/range)
            #,(mul (cons offset (cdr range)))))))


;; Call a function 'producer' with one argument, which is a function
;; taking a single argument, bound to a collector function (add to
;; set). The 2nd procedure is called with the return value and
;; collection. This uses the syntax objects converted to s-expressions
;; as keys of a hash table to make the collection behave as a set.

(define (call-with-collector producer consumer)
  (call-with-values
      (lambda  ()
        (let* ((store (make-hash))
               (collector
                (lambda (thing)
                  (hash-set! store
                             (->sexp thing) ;; for equal?
                             thing)))
               (retval (producer collector)))
          (values retval (hash-map store (lambda (_ v) v)))))
    consumer))




;; SEMANTICS AFTER TREE EXPANSION.


;; These handle the grid/loop tags after code unfolding is
;; performed. The unfolding introduces new bindings for each loop
;; level.
(expressions
 ((grid ptr) #`(* ptr))  ;; In the inner loop, a grid is a pointer.
 ((loop s)   #`s)        ;; Ignore: this is only used as tag in direct tree manipulation.
)


;; TREE EXPANSION

;; ONCE

;; Push a relative expression through a binding block. This peels off
;; the outer grid index, and indexes relative to a local pointer
;; inside the loop body.

(define (expand-loop type loop-sexp dim/range)
  (call-with-collector
   
   (lambda (binding!)
     (syntax-case/r loop-sexp (grid)

       ;; Leave fully expanded grids alone.
       ;; ((grid name) #`(grid name))

       ;; Expand one level.
       ((grid name offset . o)
        (let ((bound-name
               (offset->name #'name #'offset)))
                      
          ;; collect binding for first index
          (binding! `(,type ,bound-name
                            ,(pointer-init
                              dim/range
                              #'name
                              #'offset)))
          
          ;; leave remaining indices + bound name
          #`(grid #,bound-name . o)))))

   
   (lambda (body bindings)
     (for-loop dim/range bindings body))))


;; ALL

;; This expands all 'loop' forms until all dimensions are
;; eliminated. The result for an n-dim grid is an n-times nested loop
;; tree. (If there are no boundary conditions = only a single
;; expander, the loop tree is linear.)

;; This supports multiple expanders to handle border conditions.

(define (src->statement src        ;; assigment expression
                        expanders  ;; inner clause expansion
                        type       ;; pointer type
                        dim/range  ;; dimension (name . range)
                        )
  
  ;; Generate cgen.ss syntax by expanding the 'loop' sexps once for
  ;; each dimension.
  
  (let next ((d/r dim/range)
             (s   src))

    (if (null? d/r)
        (datum->syntax #f s) ;; Done unfolding.
        (next (cdr d/r)
              (syntax-case/r s (loop) ;; Expand one loop nesting.
                ((loop . body)
                 #`(statements
                    #,@(map
                        (lambda (expand)
                          (expand type #`(loop . body) d/r))
                        expanders))))))))
               


;; TEST.

(define (src->code src)
  (src->statement src
                  `(,expand-loop)  ;; loop expanders
                  'float*          ;; FIXME: use dynamic binding / C macro
                  '((i . 400) (j . 300))
                  ))

(define (p src)
  (display
   (statement->string
    (src->code
     ;; (parse-grid-language src dim/range)))
     src))))
  
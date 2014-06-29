#lang scheme/base

;; http://okmij.org/ftp/Scheme/zipper-in-scheme.txt

(require scheme/control)

; deterministic, left-to-right map
(define (map* f l)
  (if (null? l) l
    (cons (f (car l)) (map* f (cdr l)))))

(define (depth-first handle tree)
  (cond
    ((null? tree) tree)
    ((handle tree) => (lambda (new-tree) new-tree))
    ; the node was not handled -- descend
    ((not (pair? tree)) tree) ; an atom
    (else
      (cons (car tree) 			; node name
	(map* (lambda (kid) (depth-first handle kid)) (cdr tree))))))

(define tree1 '(a (b) (c (d 1 2)) e))
(define tree2 '(z (u) (v (w 10 12)) y))

(define-struct zipper (node k))

(depth-first (lambda (node) (display node) (newline) #f) tree1)

(define (zip-tree tree)
  (reset
   (depth-first
    (lambda (tree)
      (shift f (make-zipper tree f)))
    tree)))


(define (print-tree tree)
  (do ((cursor (zip-tree tree) ((zipper-k cursor) #f)))
      ((not (zipper? cursor)))
    (display (zipper-node cursor))
    (newline)))

(define (zip-all-the-way-up zipper)
  (if (zipper? zipper)
      (zip-all-the-way-up
       ((zipper-k zipper)
        (zipper-node  zipper)))
      zipper))

(define (locate-nth-node n tree)
  (do ((i 0 (+ 1 i))
       (cursor (zip-tree tree) ((zipper-k cursor) #f)))
    ((and (= i n)
       (if (zipper? cursor) #t
	 (error "too few nodes"))) cursor)
    ))


(let ((desired-node (locate-nth-node 3 tree1)))
  (display "Replacing the node: ")
  (display (zipper-node desired-node))
  (newline)
  (zip-all-the-way-up ((zipper-k desired-node) 'xxx)))


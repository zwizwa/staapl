#lang racket/base

(require "../tools.ss")

;; Imperative core for image processing. This serves mainly as a
;; testbed for a generated C based processing core. The main ideas are
;; about improving data locality and reducing loop overhead using:
;;
;;   * automatic loop folding (algebra of programs)
;;   * image tiling
;;   * fifo tile reuse

;; In first approximation everything runs in scheme + no tiling is used.


;; MEMORY

(define reuse-stack '())
(define-struct buffer (vector refcount) #:mutable)
(define buffer-vector-size 9)
(define (make-buffer-vector) (make-vector buffer-vector-size 0))
(define (new-buffer)
  (if (null? reuse-stack)
      (make-buffer (make-buffer-vector) 1)
      (let ((p (pop! reuse-stack)))
        (set-buffer-refcount! p 1)
        p)))

(define (delete-buffer buf)
  (let ((rc (- (buffer-refcount buf) 1)))
    (if (zero? rc)
        (push! reuse-stack buf)
        (set-buffer-refcount! buf rc))))

;; STACK
(define data-stack '())
(define (drop) (delete-buffer (pop! data-stack)))
(define (lit x) (push! data-stack x))

(define (dup)
  (let ((buf (car data-stack)))
    (set-buffer-refcount! buf (+ 1 (buffer-refcount buf)))
    (push! data-stack buf)))
        
    


;; FUNCTIONALS

;; The simplest lifting operation is to create a function that
;; processes the two images on the stack and produces a third one,
;; given a 2->1 primitive function.

(define (stack-buf i)
  (buffer-vector (list-ref data-stack i)))

;; Written as macro to get a feel for the metaprogramming.
;; (define-syntax (perform stx)
;;   (syntax-case stx ()
;;     ((_ s-in s-out fn)
;;      (let ((in  (syntax->datum #'s-in))
;;            (out (syntax->datum #'s-out)))
;;        #`(let ((
 
  
(define (2->1 fn)
  (let* ((result (new-buffer))
         (r (buffer-vector result)))
    (for ((i (in-naturals))
          (a (stack-buf 1))
          (b (stack-buf 0)))
      (vector-set! r i (fn a b)))
    (drop)
    (drop)
    (lit result)))

;; This is quite straightforward to generalize. The real challenge is
;; image shifts for spatial filters. Split the problem in 2 parts:

;;   * create a single multi in -> out map for the core loop (for an
;;     infinitely large image
;;
;;   * build an iterator
;;
;;   * solve boundary conditions


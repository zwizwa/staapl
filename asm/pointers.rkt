#lang racket/base

;; Current assembly pointers
(require
 "../tools.rkt")
(provide
 pointer-get
 pointer-set!
 pointer-allot!
 pointer-push!
 pointer-pop!
 asm-pointers
 asm-pointers-init
 with-pointer


 )

(define (with-pointer ptr value thunk)
  (dynamic-wind
      (lambda () (pointer-push! ptr value)) thunk
      (lambda () (pointer-pop! ptr))))
  



;; For the imperative algos, use a hash table data structure.
(define get  hash-ref)
(define put! hash-set!)
(define table alist->hash)
(define table->alist hash->alist)

;; Assembly address environment.  The pointers use shallow binding:
;; each is a stack of values. These values are incremented during
;; assemble, and are allowed to be changed at the start of each word.

(define (asm-pointers-init)
  (table '((code 0)
           (data 0))))

(define asm-pointers
  (make-parameter (asm-pointers-init)))

(define (pointer-stack name)
  (get (asm-pointers) name))
(define (pointer-stack! name stack)
  (put! (asm-pointers) name stack))

(define (pointer-pop! name)
  (let ((s (pointer-stack name)))
    (pointer-stack! name (cdr s))
    ;; (printf "POP, TOP = ~a\n" (pointer-get name))
    (car s)))
(define (pointer-push! name val)
  (let ((s (pointer-stack name)))
    (pointer-stack! name (cons val s))
    ;; (printf "PUSH, TOP = ~a\n" (pointer-get name))
    ))

(define (pointer-set! name val)
  (let ((s (pointer-stack name)))
    (pointer-stack! name (cons val (cdr s)))))
(define (pointer-get name)
  (car (pointer-stack name)))

(define (pointer-allot! name increment)
  ;; (printf "alloting ~a ~a\n" name increment)
  (pointer-set! name
                (+ increment
                   (pointer-get name))))

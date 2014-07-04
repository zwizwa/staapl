#lang racket/base
(require racket/pretty
         "zipper-dict.rkt")

;; Test

;; Test for RPN compiler syntax transformer.  Not using syntax objects
;; to make printing easier during debug.


(define (pack-lambda instructions)
  `(lambda (p)
     ,(foldl (lambda (ins expr)
               (apply (lambda (tag value)
                        `(,tag ,value ,expr))
                      ins))
             'p
             instructions)))

;; Not fully evaluated, easier to read.
(define (pack-lambda-debug instructions)
  `(program: ,@(map cadr instructions)))
  

;; A semantics transformer for the locals syntax.  Note that this
;; starts off again with the default semantics, not the previously
;; packed one which is only for code _preeceeding_ the local
;; construct.
(define (__make-locals-pack obj pack default-pack)
  (lambda (instructions)
    `(lambda (p)
       (let ((p+ (apply ,obj p)))
         (let ((top (car p+))
               (p++ (cdr p+)))
           (apply ,(default-pack
                    ;; pack
                    instructions) p++))))))

(define ((___make-locals-pack name) obj pack default-pack)
  (lambda (instructions)
    `(lambda (p)
       (let ((p+ (apply ,obj p)))
         (let ((,name (car p+))
               (p++ (cdr p+)))
           (apply ,(pack
                    ;; pack
                    instructions) p++))))))

(define ((make-locals-pack name) obj pack default-pack)
  (lambda (instructions)
    `(lambda (p+)
       (let ((,name (car p+))
             (p++ (cdr p+)))
         (apply ,(pack
                  ;; pack
                  instructions) p++)))))



;; Imperative interface for testing.
(define *zd* #f)
(define (update-zd! fn) (begin (set! *zd* (fn *zd*)) *zd*))

(define-syntax-rule (open! . a) (update-zd! (lambda (zd) (zd-open . a))))



(define-syntax-rule (imperative (macro! fn) ...)
  (begin
    (define-syntax-rule (macro! . a)
      (update-zd! (lambda (zd) (fn zd . a)))) ...))

(imperative
 (compile! zd-compile)
 (start!   zd-start)
 (repack!  zd-repack))


(define (test1)
  (open! #f pack-lambda-debug)
  (start!   'foo)
  (compile! '(push 123))
  (compile! '(apply fn))
  (start!   'bar)
  (compile! '(push 456))
  (compile! '(apply asdf))
  (repack!  (make-locals-pack 'outer))
  (compile! '(apply def))
  (compile! '(apply ghi))
  (repack!  (make-locals-pack 'inner))
  (compile! '(apply aaa))
  (compile! '(apply bbb))

  )

(define (test)
  (open! #f pack-lambda-debug)
  (start!   'foo)
  (compile! '(push 10000))
  (repack!  (make-locals-pack '*OUTER*))
  (compile! '(push 20000))  
  (repack!  (make-locals-pack '*INNER*))
  (compile! '(push 30000)) 
  )


(define (print) (pretty-print (zd-close *zd*)))

(test)
(print)


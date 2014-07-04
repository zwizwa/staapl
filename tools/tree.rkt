;; processing trees represented as nested s-expressions (not binary
;; trees: using 'list?' instead of 'pair?').

#lang racket/base
(require
 "list.rkt"
 (lib "match.rkt"))

(provide (all-defined-out))

;; i.e. (define broem? (tag? broem))
;; matches s-expressions tagged with 'broem'
(define-syntax tag?
  (syntax-rules ()
    ((_ tag)
     (match-lambda
      (('tag . r) #t)
      (other #f)))))


;; substitute subexpression
(define (substitute matches? fn expression)
  (let down ((e expression))
    (if (matches? e)
        (fn e)
        (if (list? e)
            (map down e) e))))

(define (substitute* matches? fn expression)
  (substitute matches?
              (splash fn)
              expression))

;; substitute only the expression body
(define (substitute-body matches? fn expression)
  (substitute matches?
              (dip fn)
              expression))

;; iterated expansion with escape continuation. takes a data
;; structure and an expander that calls the escape continuation when
;; done, which returns the last successful expansion.

(define (expand/done expand-once expr)
  (call/cc 
   (lambda (return)        ;; escape continuation
     (let down ((e expr))  ;; iterate expansion
       (down (expand-once
              (lambda () (return e))
              e))))))




;; (define (flatten tree)
;;   (if (null? tree) tree
;;       (let ((f/l
;;              (lambda (cxr)
;;                ((if (list? (cxr tree))
;;                     flatten list) (cxr tree)))))
;;         (append
;;          (f/l car)
;;          (f/l cdr)))))

(define (append* lsts)
  (apply append lsts))
(define (flatten lst)
  (append*
   (map (lambda (x)
          (if (list? x)
              (flatten x)
              (list x)))
        lst)))


  

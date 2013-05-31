#lang scheme/base

(provide rule:sum=0
         rule:prod=1
         rule:>0)

(require "cpl-net.ss")

;; RULE CLASSES

;; Rule classes implement rule behaviour.  A rule is asked to act
;; whenever one of its nodes changed.



;; EQUATIONS

;; Equations act when one of the nodes is undefined.  The result is
;; code that computes a directed version of the rule at run-time.


;; Equations trigger when one value remains undefined.
(define-syntax-rule (push! stack v)
  (set! stack (cons v stack)))
(define (nodes->undefined-indices nodes)
  (let ((indices '()))
    (for ((n nodes) (i (in-naturals)))
      (when (undefined? (node-value n)) (push! indices i)))
    indices))
(define (rule-equation-bang! action nodes)
  (let ((floating (nodes->undefined-indices nodes)))
    ;; FIXME: generalize to >1 outputs
    ;; (printf "floating: ~a\n" (length floating))
    (when (= 1 (length floating))
      (let* ((i (car floating))
             (v (action i (map node-name nodes))))
        (let* ((node (list-ref nodes i))
               (name (node-name node)))
          (emit #`(#,name #,v)) ;; sequence code
          (node-assert! node (make-defined)))))))

(define-syntax-rule (define-equation-rule (name hole-index values) . body)
  (define name
    (let ((equation-action (lambda (hole-index values) . body)))
      (make-rule-class (lambda (nodes)
                         (rule-equation-bang! equation-action nodes))
                       'name))))
(define (fold-hole fn skip-index init args)
  (let tail ((args args) (i 0))
    (cond
     ((null? args) init)
     ((= i skip-index) (tail (cdr args) (add1 i)))
     (else
      (fn (car args)
          (tail (cdr args) (add1 i)))))))

(define (stx-binop op)
  (lambda (a b) #`(#,op #,a #,b)))

;; a + b + c ... = 0
(define-equation-rule
 (rule:sum=0 i args)
 #`(- #,(fold-hole (stx-binop #'+) i #'0 args)))

;; a x b x c ... = 1
(define-equation-rule (rule:prod=1 i args)
  #`(/ 1. #,(fold-hole (stx-binop #'*) i #'1 args)))



;; INEQUALITIES

(define (rule-assert-bang! op nodes)
  (when (null? (nodes->undefined-indices nodes))
    (emit #`(assert (#,op #,@(map node-name nodes))))))

(define-syntax-rule (define-assert-rule name op)
  (define name
    (make-rule-class (lambda (nodes)
                       (rule-assert-bang! #'op nodes))
                     'name)))

(define-assert-rule rule:>0 >0)

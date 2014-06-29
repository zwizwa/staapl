#lang racket/base

(require racket/base)

;; Special purpose zipper for building lambda/let expressions:
;; (state ((lambda (state) #f))) :: <node <<siblings> <path>>>

(define (stx-zip-up stx)
  (syntax-case stx ()
    ((node ((siblings ...) parent))
     #`((siblings ... node) parent))))

(define (stx-zip-down stx)
  (syntax-case stx ()
    (( (siblings ... node) parent)
     #`(node ((siblings ...) parent)))))
     
(define (stx-zipper->tree stx)
  (syntax-case stx ()
    ((node #f) #'node)
    (_ (stx-zipper->tree (stx-zip-up stx)))))

(define (stx-tree->zipper stx) #`(#,stx #f))

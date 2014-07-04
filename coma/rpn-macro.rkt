#lang racket/base

(require
 "../rpn.rkt"
 "../ns.rkt"
 "../scat.rkt"
 "op.rkt")

(provide macro
         macro:
         macro-push
         macro->dictionary
         )

;; Basic expression compilers.  The code is right folded in rpn-lambda
;; (called from rpn-parse), so we use nested let expressions here.
(define-syntax-rule (macro-push  val p sub) (let ((p ((lit val) p))) sub))

(define-syntax-rule (macro . form) (ns (macro) . form))

(define-syntax-rule (macro->dictionary compile-dict code ...)
  (rpn-parse (compile-dict    ;; dictionary compiler
              (macro)         ;; namespace
              scat-apply      ;; function = same as scat:
              macro-push      ;; immediate
              macro-push      ;; immediate program
              macro:          ;; anonymous compiler for recursive parse
              (rpn-lambda)    ;; dictionary init (only one anonymous entry)
              ) code ...))

(define-syntax-rule (macro: code ...)
  (make-word (macro->dictionary rpn:-compile code ...)))
  
              
              

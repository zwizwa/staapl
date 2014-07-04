#lang racket/base

(require
 "../rpn.rkt"
 "../ns.rkt"
 "rep.rkt"
 "stack.rkt")

(provide scat
         scat:
         scat-apply
         scat-push
         scat->dictionary)

;; compilers specific to the state data + code rep
;; DATA = stack struct
;; CODE = unary stack -> stack functions

;; Basic expression compilers.  The code is right folded in rpn-lambda
;; (called from rpn-parse), so we use nested let expressions here.

(define-syntax-rule (scat-push  val p sub) (let ((p (stack-cons val p))) sub))
(define-syntax-rule (scat-apply fn  p sub) (let ((p (fn p))) sub))


(define-syntax-rule (scat . form)
  (ns (scat) . form))

(define-syntax-rule (scat: code ...)
  (make-word (scat->dictionary rpn:-compile code ...)))

;; This macro is factored out so it's possible to pass ``quote'' as
;; the dictionary compiler for debugging.
(define-syntax-rule (scat->dictionary compile-dict code ...) 
  (rpn-parse (compile-dict   ;; dictionary compiler
              (scat)         ;; namespace
              scat-apply     ;; function 
              scat-push      ;; immediate
              scat-push      ;; immediate program
              scat:          ;; anonymous compiler for recursive parse
              (rpn-lambda)   ;; dictionary init (only one anonymous entry)            
              ) code ...))


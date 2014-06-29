#lang scheme/base

;; Pprimitive snarfing. These macros are syntax only: they do not
;; depend on name space representation.



(require
 "stack.ss"
 "rep.ss")
(require
 (for-syntax
  scheme/base
  "../tools/stx.ss"))

(provide
 as-push as-void     ;; wrap scheme function -> rpn primitive
 )



;; SNARFING

;; To steal behaviour from scheme, we need to know the number of
;; arguments and the way the arguments are re-arrange, and what to
;; do with the return value(s).

(define-syntax scheme->cat/perm
  (syntax-rules ()
    ((_ combine fn (dsta ...) (srca ...))
     (make-word
      (stack-lambda (dsta ... . stack)
        (combine (fn srca ...) stack))))))

;; Usually, just reversing the argument list is enough: this
;; preserves the argument ordering when converting PN -> RPN.

(define-syntax scheme->cat/rev
  (lambda (stx)
    (syntax-case stx ()
      ((_ combine fn args ...)
       #`(scheme->cat/perm combine fn
                           #,(stx-reverse #'(args ...))  ;; reversed
                           (args ...))))))

;; combine = cons
(define-syntax as-push
  (syntax-rules ()
    ((_ fn args ...) (scheme->cat/rev cons fn args ...))))

;; combine = begin
(define-syntax as-void
  (syntax-rules ()
    ((_ fn args ...) (scheme->cat/rev begin fn args ...))))




#lang racket/base
(require
 racket/unit
 (for-syntax
  racket/base
  "../rpn/parse-tx.rkt"
  "../ns-tx.rkt"))

(provide (all-defined-out))

;; Allow the definition of prefix parsers in module signatures.
(define-signature-form (define-syntaxes-ns stx)
  (syntax-case stx ()
    ((_ ns (id ...) expr)
     (list
      (let ((ids (for/list ((id (syntax->list #'(id ...))))
                   (ns-prefixed #'ns id ))))
        #`(define-syntaxes #,ids expr))))))

;; Defining multiple prefix subsitution patterns.  Like
;; `prefix-parsers' in rpn/main.ss but using define-syntaxes-ns from
;; above.
(define-signature-form (prefix-parsers stx)
  (syntax-case stx ()
    ((_ namespace ((name arg ...) template) ...)
     (list
      #'(define-syntaxes-ns namespace (name ...)
          (values (rpn-syntax-rules () ((_ arg ...) template)) ...))))))

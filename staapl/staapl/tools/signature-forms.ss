#lang scheme/base

;; Some convenience routines to make it easier to move macros from
;; ordinary module code to signatures.

;; Note that a sig-form-id is in the same namespace as ordinary
;; macros, so this doesn't mix with normal use.

(require scheme/unit
         (for-syntax
          scheme/base
          "../ns-tx.ss"
          ))
(provide (all-defined-out))
         
(define-signature-form (define-syntax-rule stx)
  (syntax-case stx ()
    ((_ (name . pat) expr)
     (list
      #'(define-syntaxes (name)
          (syntax-rules () ((_ . pat) expr)))))))

(define-signature-form (define-syntax stx)
  (syntax-case stx ()
    ((_ name expr)
     (list
      #'(define-syntaxes (name) expr)))))


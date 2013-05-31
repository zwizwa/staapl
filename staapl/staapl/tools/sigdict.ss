#lang scheme/base
(provide (all-defined-out))
(require scheme/unit
         (for-syntax
          scheme/base))

;; A sigdict is a collection of signatures.
(define-syntax (define-sigdict stx)
  (syntax-case stx ()
    ((_ name (sig^ ...))
     #`(define-syntax name #'(sig^ ...)))))


;; While this works together with define-sigdict, that is only so
;; because the unit signature identifies are imported explicitly in
;; this macros invokation context : they are non-hygienic.  How to fix
;; that?  I.e. how to tell define-values/invoke-unit where to put the
;; identifiers?

(begin-for-syntax
 (define (re-syntax context stx)
   (datum->syntax context (syntax->datum stx))))

(define-syntax (define/invoke-sigdict stx)
  (syntax-case stx ()
    ((_ dict^^ (unit@ ...))
     (let ((sigs (re-syntax stx (syntax-local-value #'dict^^))))
       #`(begin
           (define-compound-unit/infer combined@
             (import)
             (export #,@sigs)
             (link unit@ ...))
           (define-values/invoke-unit combined@
             (import)
             (export #,@sigs)))))))

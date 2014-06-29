#lang racket/base
(provide define-pic18-const-unit)
(require racket/pretty
         racket/unit
         "sig.ss"
         "../pic18.ss"
         (for-syntax
          racket/pretty
          racket/unit-exptime
          racket/base))

;; Constants are defined prefixed as target macros and as ordinary
;; values in the scheme namespace.  The are exposed as (an extension
;; of) a unit on which the generic PIC18 code depends.

;; Extended signature.
(define-syntax-rule (define-pic18-const-signature macro^ id^ words)
  (begin
    (define-macro-set macro^ extends pic18-const^    words)
    (define-signature id^    extends pic18-const-id^ words)))

;; Top form.
(define-syntax (define-pic18-const-unit stx)
  (syntax-case stx ()
    ((_ sig^ sig-id^ unit@ (name val) ...)
     (let-values (((_1 words _3 _4)
                   (signature-members #'pic18-const-id^ stx)))
       ;; (pretty-print vs)
       #`(begin
           (define-pic18-const-signature sig^ sig-id^ #,words)
           (define-unit unit@
             (import)
             (export sig^ sig-id^)
             (begin (define name val) ...)
             (compositions (macro) macro: (name ',name) ...)))))))

    

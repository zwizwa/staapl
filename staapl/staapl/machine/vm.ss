#lang scheme/base

(require
 (for-syntax "vm-stx.ss"
             scheme/base))

(provide (all-defined-out))

;; (define-syntax (m-k/lambda stx)
;;   (syntax-case stx ()
;;     ((_ k formals . forms)
;;      (machine-lambda #'k
;;                      (syntax->datum #'formals)
;;                      #'forms))))

;; (define-syntax (m-k/match-lambda stx)
;;   (syntax-case stx ()
;;     ((_ k formals . forms)
;;      (machine-match-lambda #'k
;;                            (syntax->datum #'formals)
;;                            #'forms))))

;; (define-syntax-rule (CEK . body) (m-k/lambda values (C E K) . body))


;; Macros are named 'mu-...' for Machine Update.

;; Updates for structure types.

(define-syntax (mu-struct stx)
  (syntax-case stx ()
    ((_ state (struct-id registers) form)
     (machine-update-struct-tx #'state
                               #'struct-id
                               #'registers
                               #'form))))

(define-syntax-rule (mu-lambda-struct . a)
  (lambda (state) (mu-struct state . a)))


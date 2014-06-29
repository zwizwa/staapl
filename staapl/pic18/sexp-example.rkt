#lang scheme/base
(require "../pic18.ss")
(provide (all-defined-out))

;; Bypass the Forth syntax.

;; The code below is a valid Staapl module.

;; This uses the label^ sig which defines the wrappers.
;; It can also use functions directly from code.ss

;; Macros can be defined directly as:
(define macro/s-simple-macro (macro: 1 2 3))

;; The following supports nested macros with ';' in the middle of the
;; word, used in Forth syntax to make it easier to switch between
;; called and inlined words.  It requires the word to be terminated by
;; ';'.
(define macro/s-macro
  (label:wrap-macro 's-macro
                    #f
                    (macro: 1 2 3 |;|)))

;; Target words have a couple of components:                            
(define-values
  (   target/s-word   ;; target word structure
       macro/s-word   ;; call macro
   postponed/s-word)  ;; postponed code
  (label:wrap-word 's-word #f (macro: 4 5 6 |;|)))
(label:append! postponed/s-word)


;; Instantiate all postponed code.
(compile!)


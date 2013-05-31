#lang scheme/base

;; Coma units linked to partial-evaluation only.  This is an
;; intermediate module mainly for testing the core signature
;; implementations.

(require
 "../tools.ss"
 "stack-test-unit.ss"
 "code-unit.ss"
 "comma-unit.ss"
 "../forth/forth-lex.ss")

(require/provide
 "macro.ss")
(provide (all-defined-out))

(define/invoke
  (stack^ comma^ code^)
  (stack-test@ comma@ code@))
    


(define-syntax-rule (macro> . code)
  (print-macro-code (macro: . code)))
(define-syntax-rule (forth-compile str)
  (forth-lex-string/cps macro> str))

#lang racket/base

(require "main.rkt"
         "../ns.rkt"
         (for-syntax
          racket/base
          "parse-tx.rkt"))
          
(provide (all-defined-out))
;; Defining multiple prefix subsitution patterns.
(define-syntax-rule (prefix-parsers namespace ((name arg ...) template) ...)
  (ns namespace
      (define-syntaxes (name ...)
        (values (rpn-syntax-rules () ((_ arg ...) template)) ...))))

;; Like 'prefix-parsers', but translate code using a different
;; compiler and splice it in.

(define-syntax-rule (prefix-parsers/meta ns lang: (pat code) ...)
  (begin
    ;; Evaluation the pattern to check if the names are actually
    ;; defined, but that doesn't work because it includes pattern
    ;; names as well..
    ;; (begin (lang: . code) ...) ;; test-eval it
    (prefix-parsers ns (pat (,(lang: . code))) ...)))


; (prefix-parsers (macro ((foo) (bar))))

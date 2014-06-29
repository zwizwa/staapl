#lang racket/base
(require "cplt.ss"
         (planet dherman/c:3:2))
         

;; Simple `let*' code generator.

;; Produce C code fragments in serialized dataflow form (SSA).  At
;; this point, all variable names will be translated directly to C
;; form.

(define (let*->c stx [T "float"])
  (syntax-case stx ()
    ((_ ((name expr) ...) return-expr)
     (make-stmt:block
      (for/list ((n (syntax->datum #'name))
                 (e (syntax->datum #'expr)))
        (make-
       
       
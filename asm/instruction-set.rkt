;; This defines some assembler macros, including the macro
;; 'instruction-set' which generates generates symbolic <-> binary
;; translators from an instruction set specification.

#lang racket/base


(require
 "../ns.rkt"
 "pointers.rkt"
 "../tools.rkt"
;; "asm-lambda.rkt"
 "../op.rkt"
 "directives.rkt"
 (for-syntax racket/base
             "asm-lambda-tx.rkt")
 (for-template
  "../ns.rkt"
  ))

(provide
 instruction-set)


(define-syntax (instruction stx)
  (syntax-case stx ()
    ((_ name formals . body)
     (let-values
         (((asm-body
            dasm-body) (asm/dasm-lambda-tx stx)))
       #`(define-op name formals #,asm-body #,dasm-body)))))

(define-syntax-rule (instruction-set (ins ...) ...)
  (begin (instruction ins ...) ...))




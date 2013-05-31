;; This defines some assembler macros, including the macro
;; 'instruction-set' which generates generates symbolic <-> binary
;; translators from an instruction set specification.

#lang scheme/base


(require
 "../ns.ss"
 "pointers.ss"
 "../tools.ss"
;; "asm-lambda.ss"
 "../op.ss"
 "directives.ss"
 (for-syntax scheme/base
             "asm-lambda-tx.ss")
 (for-template
  "../ns.ss"
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




#lang racket/base

;; Operations interracting with the assembler context (address allocation).


(require
 "../op.rkt"
 "../ns.rkt"
 "../tools.rkt"
 "pointers.rkt"
 "dasm.rkt"
 (for-syntax
  "../op/static.rkt"
  racket/base)

 )

(provide
 (op-combine-out allot-code allot-data))

(define-syntax-rule (asm! body ...) (begin body ... '()))

(define-lowlevel-op (allot-data  addr n) (asm! (pointer-allot! 'data n)))
(define-lowlevel-op (allot-code  addr n) (asm! (pointer-allot! 'code n)))


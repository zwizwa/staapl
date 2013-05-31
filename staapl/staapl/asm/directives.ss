#lang scheme/base

;; Operations interracting with the assembler context (address allocation).


(require
 "../op.ss"
 "../ns.ss"
 "../tools.ss"
 "pointers.ss"
 "dasm.ss"
 (for-syntax
  "../op/static.ss"
  scheme/base)

 )

(provide
 (op-combine-out allot-code allot-data))

(define-syntax-rule (asm! body ...) (begin body ... '()))

(define-lowlevel-op (allot-data  addr n) (asm! (pointer-allot! 'data n)))
(define-lowlevel-op (allot-code  addr n) (asm! (pointer-allot! 'code n)))


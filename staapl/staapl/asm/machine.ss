#lang typed-scheme

;; Eventually the assembler should include a complete model of the
;; machine's execution engine, which combined with a memory model
;; (including memory mapped io devices) can fully simulate code.

;; The primary relation we're interested in now is the equivalence
;; relation BI <-> SI for asm/dasm.

;; BI = binary instruction
;; SI = symbolic instruction
;;  I = instruction (modulo equivalence, represented by SI)

;; asm  : SI -> BI
;; dasm : BI -> SI
;; sim  : SI -> machine -> machine

(define-type-alias Dictionary (Symbol -> Number))
(define-type-alias Machine (Listof Number))

(define-type-alias Operand (U Nothing (Dictionary -> Number)))

(define-struct: Bitfield ([sign : Number] [width : Number] [value : Number]))

(define-struct: I:addwf ([f : Operand] [d : Operand] [a : Operand]))

;; For dummy implementations.
(define-struct: Nothing ())
(define nothing (make-Nothing))


'(: A:addwf (I:addwf -> Bitfield))
(: D:addwf (Bitfield -> I:addwf))
(: E:addwf (I:addwf -> (Machine -> Machine)))


;; Conversion from real numbers -> bitfield.
(define bf make-Bitfield)

'(define (A:addwf b)
  (append-bitfield
   (bf -1 1 0)
   (bf -1 1 0)
   (bf -1 1 0)))

(define (D:addwf b) (make-I:addwf nothing nothing nothing))
(define ((E:addwf i) machine) machine)



#lang racket

;; ARM Forth language

(require
 "../tools.rkt"
 "../ns.rkt"
 "../macro.rkt"
 )

(require/provide
 "sig.rkt"
 "asm.rkt"
 "arm-forth-unit.rkt"
 "arm-macro-unit.rkt"
 "arm-compiler-unit.rkt"
 "../code.rkt"
 "../asm.rkt"
 "../coma/macro-forth.rkt"
 "../coma/macro-forth-sig.rkt"
 ;; Are these really necessary?
 "../scat.rkt"
 "../coma/macro.rkt"
 "../coma/macro-eval.rkt"
 "../sig.rkt"

 
 "../coma/comma-unit.rkt"       ;; comma^
 "../coma/code-unit.rkt"        ;; code^
 "../control/control-unit.rkt"  ;; control^ 
 "../comp/compiler-unit.rkt"    ;; jump^
 "../label-unit.rkt"
 "../label-sig.rkt"
) 


(provide
 (all-defined-out))

;; Need to implement label^ before this will work
;(define-dasm-collection dasm-collection)
;(define/invoke (macro-forth^) (arm-forth@))

;; The full pic18 dictionary.
(define-sigdict arm^^
  (stack^
   ram^
   comma^
   code^
   jump^
   cjump^
   control^
   cfg^
   rstack^
   org^
   machine^
   instantiate^
   label^
   compiler^
   macro-forth^
   arm-assembler^
   ))

(define/invoke-sigdict arm^^
  (;; Shared
   comma@
   code@
   compiler@
   control@
   label@
   ;; Specific
   arm-macro@
   arm-compiler@
   arm-forth@
   ))

;; This non-hygienic form collects all disassembler functions visible
;; in this module namespace.  This is used during live interaction.
(define-dasm-collection dasm-collection)



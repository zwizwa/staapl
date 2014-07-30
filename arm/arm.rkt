#lang racket

;; ARM Forth language

(require
 "../tools.rkt"
 "../ns.rkt"
 "../macro.rkt"
 )

(require/provide
 "asm.rkt"
 "macro.rkt"
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
   
   ;; Target code CFG support
   label^
   
   ;; top-level code compilation
   compiler^

   ;; Prefix parser Forth syntax
   macro-forth^
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

;; OLD NOTES see old/

;; Getting to know the ARM architecture.
;; Best place to start is to write a small frontend for the assembler.

;; Goal: make something that can actually run, say an initialization
;; sequence as is used in the OpenOCD debugger:

;; 1. Start with an assembly file + linker script that actually runs
;; on a bare-bones AT91SAM7 (ARM7TMDI)

;; 2. Generate it from s-expression
;; ...

;; Currently the low-level part is moved to libprim/arm
;; Sun Jun 12 16:02:47 CEST 2011



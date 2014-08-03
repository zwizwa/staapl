#lang racket/base

;; Provide all shared PIC18 code, with units not linked.


(require
 "../tools.rkt")

(require/provide
 "../tools.rkt"

 ;; Concatenative macro languge.
 "../coma/macro.rkt"
 
 
 ;; ASSEMBLER
 "../asm.rkt"
 "asm.rkt"



 ;; LANGUAGE

 ;; Generic interfaces + implementations
 "../sig.rkt"                   ;; generic language
 
 "../coma/comma-unit.rkt"       ;; comma^
 "../coma/code-unit.rkt"        ;; code^
 "../control/control-unit.rkt"  ;; control^ 
 "../comp/compiler-unit.rkt"    ;; jump^
 "../label-sig.rkt"   
 "../label-unit.rkt"
 
 ;; PIC18 interfaces + implementations
 "pic18-macro-unit.rkt"
 "pic18-control-unit.rkt"
 "sig.rkt"             ;; pic18 language


 ;; COMPILER + ASSEMBLER
 "pic18-compiler-unit.rkt"
 

 ;; FORTH SYNTAX
 "../coma/macro-forth-sig.rkt"
 "../coma/macro-forth-unit.rkt"
 "../coma/macro-forth.rkt"         ;; rpn prefix parsing words bound to macro.ss
;; "pic18/forth.rkt"              ;; PIC18 specific

 ;; TARGET CODE HANDLING
 "../code.rkt"

 ;; SHALLOW COROUTINES
 "scr-unit.rkt"
 
 )

(require
 (for-syntax
  racket/base
  racket/unit))


;; The full pic18 dictionary.
(define-sigdict pic18^^
  (stack^
   stack-extra^
   memory-extra^
   ram^
   comma^
   comma-extra^
   code^
   jump^
   cjump^
   control^
   cfg^
   rstack^
   org^
   machine^
   instantiate^
   postproc^
   
   ;; Target code CFG support
   label^
   
   ;; PIC18 specific language
   pic18-assembler^
   pic18-extra^
   pic18-const^    ;; macros
   pic18-const-id^ ;; Scheme identifiers

   ;; top-level code compilation
   compiler^

   ;; Prefix parser Forth syntax
   macro-forth^

   ;; State extension words.
   state-tag^

   ;; Shallow coroutines
   scr^
   ))



;; This unit provides only part of the spec.  Everything except
;; machine constants, which need to be linked in (see pic18.ss)

(define-compound-unit/infer pic18-generic@

  ;; User-provided configuration.
  (import
   pic18-const^
   pic18-const-id^)

  (export

   ;; Machine-independent base language
   stack^
   stack-extra^
   memory-extra^
   ram^
   comma^
   comma-extra^
   code^
   jump^
   cjump^
   control^
   cfg^
   rstack^
   org^
   machine^
   instantiate^
   postproc^
   
   ;; Target code CFG support
   label^
   
   ;; PIC18 specific language
   pic18-assembler^
   pic18-extra^

   ;; top-level code compilation
   compiler^

   ;; Prefix parser Forth syntax
   macro-forth^

   ;; State extension words.
   state-tag^

   ;; Shallow coroutines
   scr^

   )

   
          
  (link

   comma@
   code@
   compiler@
   control@
   label@
   
   ;; PIC18 specific code.
   pic18-macro@
   pic18-control@
   
   pic18-compiler@

   ;; Link unit for Forth syntax
   macro-forth@

   scr@
   ))
   

(provide (all-defined-out))

;; INTERACTION
(define (target-byte-address addr realm)
  (case realm
    ((code) (* 2 addr))
    ((data) addr)))


;; DISASSEMBLER

;; This non-hygienic form collects all disassembler functions visible
;; in this module namespace.  This is used during live interaction.
(define-dasm-collection dasm-collection)

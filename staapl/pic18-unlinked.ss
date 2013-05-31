#lang scheme/base

;; Provide all shared PIC18 code, with units not linked.


(require
 "tools.ss")

(require/provide
 "tools.ss"

 ;; Concatenative macro languge.
 "coma/macro.ss"
 
 
 ;; ASSEMBLER
 "asm.ss"
 "pic18/asm.ss"



 ;; LANGUAGE

 ;; Generic interfaces + implementations
 "sig.ss"                   ;; generic language
 
 "coma/comma-unit.ss"       ;; comma^
 "coma/code-unit.ss"        ;; code^
 "control/control-unit.ss"  ;; control^ 
 "comp/compiler-unit.ss"    ;; jump^
 "label-sig.ss"   
 "label-unit.ss"
 
 ;; PIC18 interfaces + implementations
 "pic18/pic18-macro-unit.ss"
 "pic18/pic18-control-unit.ss"
 "pic18/sig.ss"             ;; pic18 language


 ;; COMPILER + ASSEMBLER
 "pic18/pic18-compiler-unit.ss"
 

 ;; FORTH SYNTAX
 "coma/macro-forth-sig.ss"
 "coma/macro-forth-unit.ss"
 "coma/macro-forth.ss"         ;; rpn prefix parsing words bound to macro.ss
 "pic18/forth.ss"              ;; PIC18 specific

 ;; TARGET CODE HANDLING
 "code.ss"

 ;; SHALLOW COROUTINES
 "pic18/scr-unit.ss"
 
 )

(require
 (for-syntax
  scheme/base
  scheme/unit))


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

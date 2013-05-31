;; PIC18 compiler core test without Forth syntax and CFG compiler.

;; This module is used in the documentation.

#lang scheme/base
(require
 "../tools.ss")
(require/provide
 "../sig.ss"
 "sig.ss"
 "../target.ss"
 "../tools.ss"
 
 "../coma/comma-unit.ss"
 "../coma/code-unit.ss"
 "../control/control-unit.ss"
 "../control/jump-test-unit.ss"
 "pic18-unit.ss"
 "pic18-control-unit.ss"

 "../coma/macro-forth.ss"

 "../forth/forth-lex.ss"

 ;; forth
 "../ns.ss"
 "../rpn.ss"

 "../macro.ss"

 "pic18-const.ss"
 "asm.ss"

 "../scat.ss"
 "../asm.ss"
 )

(provide
 (all-defined-out))

(define/invoke
  (stack^
   stack-extra^
   memory-extra^
   comma^
   code^
   jump^
   cjump^
   control^
   machine^
   pic18-assembler^
   pic18-extra^
   pic18-postproc^)
  (pic18@
   comma@
   code@
   control@
   jump-test@
   ))

(target-print-word-bytes 2) 
(target-print-address-bits 16)


(define (optimize-state optimizer state)
  (macro-list->state
   (for/list ((macro (state->macro-list state)))
     (macro: ,macro ,optimizer))
   state:stack))

(define (optimize state . optimizers)
  (foldl optimize-state state optimizers))

(define-syntax-rule (code> . code)
  (state-print-code
   ((macro: . code) (state:stack))))

(define-syntax-rule (pic18> . code)
  (state-print-code
   (optimize ((macro: . code) (state:stack))
             (macro: pseudo)
             (macro: opti-save))))

  
(define (help)
  (display "Use the pic18> form to compile Forth code to intermediate form.\n"))


;; snot
(define-syntax-rule (forth-compile str)
  (forth-lex-string/cps macro> str))


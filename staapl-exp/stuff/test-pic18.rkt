;; PIC18 compiler core test without Forth syntax.

#lang racket/base
(require
 "../sig.ss"
 "../target.ss"
 "../tools.ss"
 
 "../coma/comma-unit.ss"
 "../coma/code-unit.ss"
 "../control/control-unit.ss"
 "../comp/compiler-unit.ss"
 "pic18-unit.ss"
 "pic18-control-unit.ss"

 "../comp/debug.ss"
 "../coma/macro-forth.ss"

 "../forth/forth-lex.ss"

 ;; forth
 "../ns.ss"
 "../rpn.ss"
 )

(provide
 (all-defined-out))

;; (define-unit test@
;;   (import stack^ forth^)
;;   (export)
;;   (macro-forth-begin : abc 1 +))

(define/invoke
  (stack^ stack-extra^ comma^ code^ jump^ control^ cfg^ rstack^ org^ machine^ instantiate^
   memory-extra^)
  (pic18@
   pic18-control@
   comma@
   code@
   control@
   compiler@
   ))

(target-print-word-bytes 2) 
(target-print-address-bits 16)



;(define-syntax-rule (forth-compile str)
;  (forth-lex-string macro-forth-begin str))

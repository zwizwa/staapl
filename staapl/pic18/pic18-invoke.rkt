#lang racket/base

;; This module provides a non-hygienic macro that links the pic18
;; module with chip-specific information and exposes all the signature
;; identifiers and macros into the current module namespace.

;; In short: It bundles the zoo of units into an easy-to-use module.

;; The trick seems to be to find out where `define-values/invoke-unit'
;; decides where to put the identifiers (main form), and how it refers
;; to other names (unit refs are higienic).  See the comments marked
;; "***" below.

;; I can't say I understand why the signatures themselves do not need
;; to be hygienic.  I don't know what's up with the label-sig either.
;; I had to put it in the non-hygienic part.  The only difference with
;; other sigs is that this one has macros.

(provide pic18-invoke)
(require (for-syntax racket/base))

(require
; (for-template
;  "../label-sig.rkt"
;  "sig.rkt")
  
 
 "../tools.rkt"
 
 ;; ASSEMBLER
; "../asm.rkt"
; "asm.rkt"
 
 
 ;; LANGUAGE/COMPILER

 ;; Generic interfaces + implementations
 "sig.rkt"                      ;; pic18 language
 "../sig.rkt"                   ;; generic language
 
 "../coma/comma-unit.rkt"       ;; comma^
 "../coma/code-unit.rkt"        ;; code^
 "../control/control-unit.rkt"  ;; control^ 
 "../comp/compiler-unit.rkt"    ;; jump^
 "../label-sig.rkt"   
 "../label-unit.rkt"
 
 ;; PIC18 interfaces + implementations
 "sig.rkt"
 "pic18-macro-unit.rkt"
 "pic18-control-unit.rkt")
          

(define-syntax pic18-invoke
  (lambda (stx)
    (syntax-case stx ()
      ((_ config@ ...)
       ;; *** 1. The outer body of the expression assumes the lexical
       ;; context of the invokation site.  This results in identifiers
       ;; being introduced in the correct place by
       ;; `define-values/invoke-unit'.
       
       (datum->syntax
        stx
        `(begin
           (define/invoke
             (;; Machine-independent base language
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
              pic18-const^
              pic18-assembler^
              pic18-extra^)

             ;; *** 2. The unit references themselves are hygienic, as
             ;; they are from the definition site of the macro.
             
             ,#'(;; Machine-independent code.
                 comma@
                 code@
                 compiler@
                 control@
                 label@
                   
                 ;; PIC18 specific code.
                 pic18-macro@
                 pic18-control@
                   
                 ;; User-provided target specifics + library configuration.
                 config@ ...
                 ))))))))

;;test
;(require "pic18-const-unit.rkt")
;(pic18-invoke pic18-const@)

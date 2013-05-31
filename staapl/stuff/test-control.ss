#lang scheme/base

;; Test module linking control unit to jump^ implemented in terms of
;; flat assembler output, and stack^ with partial evaluation only.

(require
 "../sig.ss"
 "../tools.ss"
 "2stack.ss"
 "../macro.ss"

 ;; the unit we want to test
 "control-unit.ss"

 ;; stub units for testing
 "../coma/stack-test-unit.ss"  
 "jump-test-unit.ss"
 "cjump-test-unit.ss"

 "../forth/forth-lex.ss")

(define/invoke
  (stack^ jump^ control^ cjump^)
  (control@
   stack-test@
   cjump-test@
   jump-test@))


;; For testing Coma+Control macros using the 'macro>' Scheme macro
;; with only 2stack state (not the full compiler state). This uses
;; [label ...]  and [jw/if ...] pseudo ops. See comp.ss for the real
;; work.

(define-syntax-rule (macro> . code)
  (state-print-code ((macro: . code) (state:2stack))))

;; Use the lexer on top to use command line interface to the compiler.

(define-syntax-rule (forth-compile str)
  (forth-lex-string/cps macro> str))

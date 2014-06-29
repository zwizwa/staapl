;; Forth compiler core.

#lang racket/base
(require
 "../sig.ss"
 "../tools.ss"

 ;; the unit under test
 "compiler-unit.ss" ;; export: jump^ instantiate^

 ;; functional units
 "../coma/comma-unit.ss"
 "../coma/code-unit.ss"
 "../control/control-unit.ss"

 ;; stub units for testing
 "../coma/stack-test-unit.ss"
 "../comp/machine-test-unit.ss"
 "../control/cjump-test-unit.ss"

 "../comp/debug.ss"   ;; macro> forth-compile
 )


(provide
 (all-defined-out))

(define/invoke
  (stack^ comma^ code^ jump^ control^ cfg^)

  (stack-test@
   machine-test@
   cjump-test@
   
   comma@
   code@
   control@
   compiler@))

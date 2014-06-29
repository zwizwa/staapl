#lang scheme/unit

;; This contains the base coma language and support code.

;;  * partial evaluation
;;  * bindings to code compilation (labels)
;;  * basic Forth control macros

(require
 scheme/unit
 "../sig.ss"
 "../coma/macro.ss")


(import)
(export machine^)
(compositions
 (macro) macro:
 (address)
 (code-size 1))
  

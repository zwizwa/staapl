#lang racket/unit

;; This contains the base coma language and support code.

;;  * partial evaluation
;;  * bindings to code compilation (labels)
;;  * basic Forth control macros

(require
 racket/unit
 "../sig.rkt"
 "../coma/macro.rkt")


(import)
(export machine^)
(compositions
 (macro) macro:
 (address)
 (code-size 1))
  

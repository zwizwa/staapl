#lang scheme/base

;; Macro Forth for PIC18.

(require "tools.ss")
(require/provide
 "pic18-unlinked.ss"         ;; All PIC18 code, not linked.
 "pic18/pic18-const-unit.ss" ;; Chip-specific config.
)

(provide (all-defined-out))

(define/invoke-sigdict pic18^^ 
  (pic18-const@    ;; constants provided by us
   pic18-generic@  ;; generic PIC18 code
   ))


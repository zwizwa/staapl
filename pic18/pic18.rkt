#lang racket/base

;; Macro Forth for PIC18.

(require "../tools.rkt")
(require/provide
 "pic18-unlinked.rkt"   ;; All PIC18 code, not linked.
 "pic18-const-unit.rkt" ;; Chip-specific config.
)

(provide (all-defined-out))

(define/invoke-sigdict pic18^^ 
  (pic18-const@    ;; constants provided by us
   pic18-generic@  ;; generic PIC18 code
   ))

(define macro/target (macro: 'pic18))


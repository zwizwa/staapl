#lang racket/base

;; Assembler/disassembler functions need extra context information.
(provide (all-defined-out))

;; Note that the pointers for current code and data location +
;; operations are defined separately in pointers.ss
(require "pointers.rkt")

;; Current assembler phase.  In phase 0 most errors are suppressed to
;; allow the first pass to succeed.
(define asm-phase (make-parameter -1))

;; Context-sensitive error handling.
(define asm-error (make-parameter
                   (lambda a (raise a))))

;; Offset computation for relative addressing. The following seems to
;; be a standard: relative to the PC after the jump instruction.  The
;; default is context-sensitive, but it's possible to override by
;; passing in the PC manually.
(define (asm-offsetter op)
  (lambda (addr [PC (pointer-get 'code)])
    (op addr (+ PC 1))))

(define asm-offset  (make-parameter (asm-offsetter -)))
(define dasm-offset (make-parameter (asm-offsetter +)))


;; For error reporting.
(define asm-current-word (make-parameter #f))
(define asm-current-chain (make-parameter #f))
(define asm-current-instruction (make-parameter #f))


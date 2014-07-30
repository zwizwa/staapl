#lang racket/base
(require
 "../sig.rkt"
 ;; "sig.rkt"
 "../coma/macro.rkt"
 "../control/op.rkt"
 "asm.rkt")

(provide (all-defined-out))

(patterns
 (macro)

 ;; RPN assembler
 (([qw d] [qw s] [qw i] addi) ([addi d s i]))
 (([qw d] [qw s] [qw i] subi) ([addi d s i]))
 (([qw d] [qw s] [qw n] add)  ([add  d s n]))
 (([qw d] [qw s] [qw n] sub)  ([add  d s n]))

 )




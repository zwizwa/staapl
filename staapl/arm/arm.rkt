#lang racket

;; Getting to know the ARM architecture.
;; Best place to start is to write a small frontend for the assembler.

;; Goal: make something that can actually run, say an initialization
;; sequence as is used in the OpenOCD debugger:

;; 1. Start with an assembly file + linker script that actually runs
;; on a bare-bones AT91SAM7 (ARM7TMDI)

;; 2. Generate it from s-expression
;; ...

;; Currently the low-level part is moved to libprim/arm
;; Sun Jun 12 16:02:47 CEST 2011



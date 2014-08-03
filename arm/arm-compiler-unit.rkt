#lang racket/unit

(require
 racket/pretty
 "../sig.rkt"
 "asm.rkt"
 "../asm/assembler.rkt"
 "../asm.rkt"
 "../code.rkt")

(import instantiate^ postproc^)
(export compiler^)

(define (compile!)
  (code-compile! compile-words  ;; instantiate^
                 postproc       ;; postproc^
                 assemble!))    ;; assembler code

;; FIXME: see arm/qemu
(code-pointers-set!
 '((code #x1000)
   (data #x2000)))


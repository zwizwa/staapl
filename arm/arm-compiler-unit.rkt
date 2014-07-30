#lang racket/unit

(require
 "../sig.rkt"
 "asm.rkt"
 "../asm/assembler.rkt"
 "../asm.rkt"
 "../code.rkt")

(import instantiate^) ;; postproc^)
(export compiler^)

(define (postproc . a) (error 'postproc))

(define (compile!)
  (code-compile! compile-words  ;; instantiate^
                 postproc       ;; postproc^
                 assemble!))    ;; assembler code

(code-pointers-set!
 '((code #x0022)
   (data #x0000)))

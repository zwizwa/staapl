#lang scheme/unit

(require
 "../sig.ss"
 "asm.ss"
 "../asm/assembler.ss"
 "../asm.ss"
 "../code.ss")

(import instantiate^ postproc^)
(export compiler^)

;; Bind compiler components.
(define (compile!)
  (code-compile! compile-words  ;; instantiate^
                 postproc       ;; postproc^
                 assemble!))    ;; assembler code

;; ALLOT STACKS

;; These are currently not settable from assembly code due to
;; dependence on the order of module instantiation and "load"
;; statements.  It is only possible to change them locally (using
;; "org-begin" and "org-end") for the purpose of installing vectors.
;; We start at #x0022 because the first #20 words are reserved as
;; separately erasable block0, and the start of block1 contains a
;; 2-word slot for a jump to boot code to which the default boot code
;; will jump.

(code-pointers-set!
 '((code #x0022)
   (data #x0000)))

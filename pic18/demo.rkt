#lang racket/base
(require "../tools.rkt")
(require/provide
 "../rpn.rkt"
 "../pic18.rkt")
(provide (all-defined-out))



;; Ad-hoc PIC18 compiler demo.  This is used to demonstrate the basic
;; idea behind the `macro' representation.

;; It is not very useful for practical work as it doesn't include
;; control flow, which requires a wider control state.

;; Target code printer config.
(target-print-word-bytes 2) 
(target-print-address-bits 16)
(target-print-max-ins-words 2)


;; For this demo we use a simple printing method for showing the
;; effect of a code generation/processing macro that does not use the
;; control flow stack.
(define (print-macro macro [proc (lambda (lst) lst)])
  (state-print-code
   (state-postproc (macro (state:stack)) proc)))

;; The PIC18 compiler needs a postprocessing step that's represented
;; by the `postproc' method, which operates on a reversed list of
;; assembly code == the stack of a compilation state.  This routine
;; maps that postproc routine over a state object, defaulting to no
;; postproc.
(define (state-postproc state proc-list)
  (make-state:stack (proc-list (state->stack state))))


;; These macros are used in the manual.
(define-syntax-rule (pic18> . words) (print-macro (macro: . words) postproc))
(define-syntax-rule (code>  . words) (print-macro (macro: . words)))

;; Full forth demonstration
(define-syntax-rule (forth> str)
  (begin
    (with-handlers
        ((void (lambda (ex)
                 (printf "~a\n" ex)
                 (code-clear!))))
      (forth-compile str)  ;; This does not include '(library "pic18") !!
      (code-print)
      (code-clear!))))

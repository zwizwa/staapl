#lang scheme/base

;; The 2-stack model extends the 1-stack Coma language with a stack to
;; implement Forth-style control words. Coma together with higher
;; order macros is enough to build control structures. However, that
;; is currently (Tue Jul 15 12:42:58 CEST 2008) not yet used.

;; The monitor code for interactive Purrr development is implemented
;; in Purrr, which is a Forth-style imperative language with
;; Forth-style control words (like if ... else .. then). Those need an
;; extra stack to hold jump labels.

;; Note that the Purrr dialect also supports multiple entry and exit
;; points, and compiles to a structured code graph, which requires a
;; bit more compiler state (macro return stack + dictionary of code
;; chains).

(provide
 2stack
 2stack?
 (ns-out (scat) (combine-out  >ctrl ctrl> ctrl-swap))


 
 ;; (rename-out (new-2stack make-2stack)) ;; only for drivers
 make-state:2stack
 state:2stack

 2stack-asm-list
 2stack-ctrl-list

)

(require
 "../ns.ss"
 "../tools.ss"
 "../scat.ss"
 scheme/match
 )

;; The second stack is the control stack for the Forth style control
;; flow words.

(define-struct (2stack stack) (ctrl-list))
(define 2stack-asm-list stack-list)  ;; scat stack is macro control stack


(define (make-state:2stack asm ctrl)
  (make-2stack
   (case-lambda
     ((state asm)
      (make-state:2stack asm (2stack-ctrl-list state)))
     ((state asm ctrl)
      (make-state:2stack asm ctrl)))
   asm ctrl))

(define (state:2stack [datastack '()])
  (make-state:2stack datastack '()))

;; Data transfer between control stack and asm stack.
(ns (scat) (define >ctrl
             (state-lambda 2stack
                           ((list-rest x asm+) ctrl)
                           (update asm+ (cons x ctrl)))))

(ns (scat) (define ctrl>
             (state-lambda 2stack
                           (asm (list-rest x ctrl+))
                           (update (cons x asm) ctrl+))))

(ns (scat) (define ctrl-swap
             (state-lambda 2stack
                           (asm (list-rest a b ctrl+))
                           (update asm (list* b a ctrl+)))))



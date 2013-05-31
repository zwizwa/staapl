;; PIC30 code generator
#lang scheme/unit
(require
 "../sig.ss"
 "../coma/macro.ss"
 "../control/op.ss"
 "asm.ss")

(import)
(export stack^
        stack-extra^
        memory-extra^
        machine^
        cjump^
        comma^
        )


(define (ni) (error 'not-implemented))
(define-syntax-rule (not-implemented: word ...)
  (patterns (macro) ((word) (ni)) ...))

(not-implemented:
 ;; stack^
 dup swap + - * / not ;; drop
 ;; stack-extra^
 and or xor pow >>> <<< 1+ 1- min max odd? toggle neg >> << 2/ over nip
 ;; comma^ comma-extra^
 ","   dw> "string,"
 ;; memory-extra^
 1-! 1+! swap! ! @ high low high? low? +! @! or! and! xor!
 ;; machine^
 code-size address
 ;; cjump^
 jw/false)



(patterns
 (macro)
 ((drop) (ni))

  
 )
 
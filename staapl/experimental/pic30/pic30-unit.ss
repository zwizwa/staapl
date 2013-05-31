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


;; Machine model.

;; The PIC24/30/33 is quite different from the 8-bit PICs as it has a
;; lot more registers.  However, in the stack machine model this file
;; implements these will be left mostly unused.  The basic goals of
;; this model are:

;;  - a 'thin' mapping
;;  - little context: optimize for task switching (stacks only)

;; This is in stark contrast with normal DSP-style operations, where a
;; number of registers are used to keep intermediate computation
;; state.  It seems to me it's probably better to use Forth where it's
;; good (task switching) and orthogonally use the registers in
;; different models for building numerical code (including
;; compile-time mapping from concatenative stack code to dataflow).

;; So, the register map, trying to be as compatible as possible with
;; Hugh's map for the 32bit floating point VM:
;; .equiv AL,  W0
;; .equiv AH,  W1  ; AL and AH are for multiplication and division
;; .equiv BL,  W2
;; .equiv BH,  W3  ; AL, AH, BL and BH are for temporary use, and in ISRs
;; .equiv GL,  W4
;; .equiv GH,  W5  ; GL and GH are local variables used within { } brackets
;; .equiv SOS, W6  ; second of parameter stack; also float arithmetic sign bit
;; .equiv TOS, W7  ; top of parameter stack
;; .equiv M0,  W8  ; float mantissa low
;; .equiv M1,  W9  ; float mantissa
;; .equiv M2,  W10  ; float mantissa
;; .equiv M3,  W11  ; float mantissa high
;; .equiv EX,  W12  ; float exponent
;; .equiv FP,  W13  ; float stack pointer
;; .equiv SP,  W14  ; parameter stack pointer
;; .equiv RP,  W15  ; return stack pointer


;; Let's keep:
;;  SP = W14  parameter stack pointer
;;  RP = W15  return stack pointer
;;
;; But use this to stay closer to the PIC18 implementation.  It's a
;; small optimization that pays off well.
;;  TOS = W0  top-of-stack pointer

(patterns
 (macro)
 ((drop) (ni))  ;; MOV [--W14], W0  ;; (mov (<--R> W14) W0)
 
 )
 
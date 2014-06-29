#lang racket/unit

;; PIC18 code generator for control flow

;; This is kept separate to be able to use the core PIC18 compiler in
;; the functional concatenative language, which has its own control
;; mechanism.

(require
 "../sig.rkt"
 "sig.rkt"
 "../coma/macro.rkt"
 "../control/op.rkt"
 "asm.rkt"
 )

(import control^
        jump^
        cfg^
        pic18-const^
        pic18-const-id^
        pic18-extra^
        memory-extra^
        pic18-assembler^)
(export rstack^)

(patterns
 (macro)

 (([qw l] jw/nz)   ([bpz 1 l])) ;; ( label -- )   ;; FIXME
 
 ((rdrop)          ([movf POSTDEC1 1 0]))


 )

(compositions
 (macro) macro:

 ;; FIXME: does this still work?
 ;; swapbra is an optimization hook for PIC18
 ;; (then    m> enter: swapbra)  

 ;; control flow
 ;; simple for..next
 (for0  >r begin)
 (next0 make-label enter:      ;; split here
        r1- m> jw/nz
        rdrop)

 (for for0)
 (next next0)

 ;; control stack
 (+r    PREINC1)
 (r-    POSTDEC1)
 (r     INDF1)
 (rl    FSR1L)
 (rh    FSR1H)

 ;; This serves as the ANS 'R' stack. (cell size)
 (>r    +r !)
 (r@    r  @)
 (r>    r- @)

 ;; target is register, not wreg
 (r1-   r 1-!)

  ;; the control stack is used to help with other data stack jugglings
 (swap>r  2nd- +r movff)
 (over>r  2nd  +r movff)
 

 
)
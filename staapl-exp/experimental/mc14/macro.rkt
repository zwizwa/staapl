#lang racket/base

;; For the 14 bit arch, the INDF register is the data stack pointer,
;; growing downwards (push = dec).

;; It's a bit less obvious whether the working reg should be top of
;; stack. In the Badnop v1 implementation it looks like wreg is used
;; for other things:

;; : 1-,   INDF decf, f, ;
;; : 1+,   INDF incf, f, ;

;; I believe this was because some operations become too cumbersome if
;; the working register is not there as temporary storage. This does
;; make non-specialized math and logic operations less efficient
;; however: they need a pop + compute + push.

(require
 ;; constants are bound as macros, and in the scheme namespace.
 ;; don't re-export constants: require target to provide its own.
 "const.ss"
 "asm.ss"
 "../target.ss"
 "../asm.ss"
 "../tools.ss"
 "../scat.ss"
 "../coma.ss"
 "../control.ss")

(provide (all-defined-out))

;; PRIMITIVE PATTERNS

(patterns
 (macro)

 ;; Stack primitives.
 ((drop)         ([incf INDF 1]))
 ((save)         ([decf INDF 1]))
 ((w<-top)       ([movf INDF 0]))
 ((top<-w)       ([movwf INDF]))
 ((w<-dp)        ([movf FSR 0]))
 ((dp<-w)        ([movwf FSR]))
 (([qw a] movlw) ([movlw a]))

 ;; Compilation to machine ops.
 (([qw a] machine)  (macro: save ',a movlw top<-w))
 (([exit] machine)  ([return]))
 ((machine)         ())

 )

;; PATTERN CLASSES

(patterns-class
 (macro)
 ;;---------------------------------
 (op    pe/op  opcode  w/op   ~op)
 ;;---------------------------------
 ((+    +      addwf   w/+    ~+) 
  (-    -      subwf   w/-    ~-)
  (and  and    andwf   w/and  ~and)
  (or   or     iorwf   w/or   ~or)
  (xor  xor    xorwf   w/xor  ~xor))
 ;;--------------------------------- 
 ((w/op)              ([opcode INDF 1]))
 (([qw a] [qw b] op)  ([qw (tscat: a b pe/op)]))
 (([qw a] op)         (macro: ',a movlw w/op))
 ((op)                (macro: ~op))
 ((~op)               (macro: w<-top drop w/op))) ;; (*)
 
;; (*) These are quite large (3 ins) so a hook is provided to make it
;; possible to override that expansion macro with library code.

 


;; Runs the listed macros in order as post optimizer steps on the
;; entire assembly code list. The last operation should eliminate all
;; intermediate representations.
(define mc14-postprocess
  (macros->postprocess (macro)
                       machine))

(target-postprocess
 mc14-postprocess)

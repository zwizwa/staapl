;; PIC18 code generator
#lang racket/unit

(require
 racket/pretty
 "../sig.rkt"
 "sig.rkt"
 "../coma/macro.rkt"
 "../control/op.rkt"
 "asm.rkt"
 "../comp/postprocess.rkt"
 )


(import pic18-const^     ;; coma macros
        pic18-const-id^) ;; ordinary scheme identifiers
        

(export stack^
        stack-extra^
        memory-extra^
        machine^
        pic18-extra^
        pic18-assembler^
        cjump^
        comma^

        postproc^
        )

;; These are used as intermediate results for assembler transforms,
;; and do not have a target implementation.
(define-virtual-ops
 (cmp?  opcode reg a d)
 (flag? opcode inverted)
 (bit?  f b p)
 (invwf f a d)
 (invlw l)
 (save)
 )



;; Simulation.

(compositions
 (scat) scat:

 (truncate #xFF and) ;; truncate data word

 ;; these make no sense in infinite precision so are truncated
 (rot<<     truncate dup 7 >>> swap 1 <<< or truncate)
 (rot>>     truncate dup 7 <<< swap 1 >>> or truncate))




;;                    *** PRIMITIVE CODE GENERATOR ***


;; UNARY OPS

(patterns-class
 (macro)
 ;;----------------------------------------
 (word         opcode)
 ;;----------------------------------------
 ((1+          incf)
  (1-          decf)
  (rot<<c      rlcf)
  (rot>>c      rrcf)
  (rot<<       rlncf)
  (rot>>       rrncf)
  (rot>>4      swapf))
 ;;----------------------------------------
 (([movf f 0 0] word) ([opcode f 0 0]))
 ((word)              ([opcode WREG 0 0])))


(patterns-class
 (macro)
 ;;----------------------------------------
 (word opcode)
 ;;----------------------------------------
 ((1-! decf)
  (1+! incf))
 ;;----------------------------------------
 (([qw f] word)       ([opcode f 1 0])))


;; BINARY OPS
            
(patterns-class
 (macro)
 ;;--------------------------------
 (word  l-opcode  s-opcode)
 ;;--------------------------------
 ((+    addlw     addwf)
  (and  andlw     andwf)
  (or   iorlw     iorwf)
  (xor  xorlw     xorwf))
 ;;---------------------------------------------------------------
 (([qw a ] [qw b] word)         ([qw (tv: a b word)]))
 (([l-opcode a] [qw b] word)    ([l-opcode (tv: a b word)]))
 (([qw a] word)                 ([l-opcode a]))
 (([save] [movf a 0 0] word)    ([s-opcode a 0 0])) ;; (1)
 ((word)                        ([s-opcode POSTDEC0 0 0])))
;; (1) binary ops combine with memory fetch: "123 @ +". only for
;; commutative ops!

(patterns-class
 (macro)
 ;;------------
 (word)
 ;;------------
 ((pow)
  (>>>)
  (<<<)
  (/)
  (*))
 ;;---------------------------------------------------------------
 (([qw a ] [qw b] word)         ([qw (tv: a b word)])))


(patterns-class
 (macro)
 ;;--------------
 (word s-opcode)
 ;;--------------
 ((++ addwfc))
 ;;-------------------------------------------------------
 (([save] [movf a 0 0] word)    ([s-opcode a 0 0])) ;; (1)
 ((word)                        ([s-opcode POSTDEC0 0 0])))
;; (1) like "dup 123 +!"


(patterns-class
 (macro)
 ;;----------------------------
 (word  opcode)
 ;;----------------------------
 ((--!  subwfb)
  (-!   subwf)
  (++!  addwfc)
  (+!   addwf)
  (and! andwf)
  (or!  iorwf)
  (xor! xorwf))
 ;;----------------------------
 (([dup] [qw f] word)  ([opcode f 1 0]))  ;; (1)
 (([qw f] word)        ([opcode f 1 0] [drop])))

          
  
;; RPN ASSEMBLER
(patterns-class
 (macro)
 opcode (movf xorwf andwf iorwf subwf subfwb addwf addwfc comf rrcf rlcf rrncf rlncf)
 ;; ---------------------------------------------------------------------------------
 (([qw f] opcode) ([opcode f 0 0])))

(patterns-class
 (macro)
 opcode (cpfseq cpfsgt cpfslt clrf setf movwf mulwf)
 ;; ------------------------------------------------
 (([qw f] opcode) ([opcode f 0])))

(patterns-class
 (macro)
 opcode (addlw xorlw andlw iorlw)
 ;; ------------------------------------------------
 (([qw k] opcode) ([opcode k])))

(patterns-class
 (macro)
 opcode (push pop sleep reset nop clrwdt daw tblrd* tblrd*- tblrd*+ tblwt* tblwt*- tblwt*+)
 ;; ---------------------------------------------------------------------------------------
 ((opcode) ([opcode])))
  

  
(patterns
 (macro)

 ;; The ' word always produces quoted macros.  This word will evaluate
 ;; a macro and return the target address if possible.  In Forth we
 ;; only use byte addresses, while the PIC18 assembler uses word
 ;; addresses.
 (([qw a] word-address) ([qw (macro->target-word a)]))
    
  

 
 ;; DEBUG
 
 ;; The syntax ",xxx" in type position means: match any instruction
 ;; type, and bind the variable xxx to the type name. The syntax
 ;; ",xxx" in expression position means: create a type represented by
 ;; the symbol bound to variable xxx.
 ((,word backspace) ())                  


   
 ;; SUBTRACT
 ;; special because it's not commutative + sublw has arguments swapped!
 ;;   (-    -         sublw       subwf)
 ;;   (--   invalid   invalid     subfwb)

 (([qw a ] [qw b] -)             ([qw (tv: a b -)]))
 (([addlw a] [qw b] -)           ([addlw (tv: a b -)]))
 (([qw a] -)                     ([addlw (tv: a -1 *)]))
   
 ;; there's no subfw
 (([save] [movf a 0 0] -)        ([bpf 0 STATUS 0 0] [subfwb a 0 0])) 
 ((-)                            ([subwf POSTDEC0 0 0]))

 (([save] [movf a 0 0] --)       ([subfwb a 0 0])) ;; (1)
 ((--)                           ([subwfb POSTDEC0 0 0]))
   

 ;; FETCH
 (([movlw a] @)  ([movf a 0 0]))  ;; register fetch
 (([qw a] @)     ([save] [movf a 0 0]))
 ;; ((@)            (macro: ~@))  ;; STUB

 

 ;; STORE
 (([qw 0]  [qw a] !) ([clrf a 0]))      ;; these 2 better done after assembly..
 (([qw -1] [qw a] !) ([setf a 0]))
 (([dup] [qw a] !)   ([movwf a 0]))     ;; dup a !
 (([qw x] [qw y] [qw a] !)
  (if (eq? x y)
      (asm-pattern [qw x] [movwf a 0])                 ;; literal DUP artifact
      (asm-pattern [qw y] [movwf a 0] [drop] [qw x]))) ;; literal commutes (FIXME: commute rules)
 (([qw a] !)          ([movwf a 0] [drop]))    ;; simple literal op
 ;; ((!)                 (macro: ~!))  ;; STUB

 ;; BANKED FETCH / STORE
; (([dup] [qw bank] b!) ([movlb bank] [dup]))  ; WRONG?
 ((      [qw bank] b!) ([movlb bank]))
 
 (([movlw a] @b) ([movf a 0 1]))
 (([qw a]    @b) ([save] [movf a 0 1]))

; (([dup] [qw a] !b) ([movwf a 1])) ; WRONG?
 ((      [qw a] !b) ([movwf a 1] [drop]))
 ((b@)              (macro: BSR @))
 
 
   
 ;; ARRAY
 (([qw lo] [qw hi] a!!) ([_lfsr 2 lo hi]))
 ((a!!)                 (macro: ~a!!))


   

 ;; STACK
 (([qw r] swap!)   ([xorwf r 0 0]   ;; swap using the 3 xor trick
                    [xorwf r 1 0]
                    [xorwf r 0 0]))

 (([qw a] dup)  ([qw a] [qw a]))
 (([drop] dup)  ([movf INDF0 0 0]))
 ((dup)         ([dup]))

 (([qw a] drop) ())
 ((drop )       ([drop]))

 (([qw a] [qw b] swap)  ([qw b] [qw a]))
 ((swap)                ([xorwf INDF0 0 0]   ;; swap using the 3 xor trick
                         [xorwf INDF0 1 0]
                         [xorwf INDF0 0 0]))


 ;; RPN ASSEMBER
 (([,opc f d a] d=reg)  ([,opc f 1 a]))
 (([,opc f d a] d=w)    ([,opc f 0 a]))
 ((return)              ([return 0]))
 (([qw a] movlw)        ([movlw a]))
 (([qw a] retlw)        ([retlw a]))
 (([qw a] sublw)        ([sublw a]))  ;; subtract W from F
 (([qw s] [qw d] movff) ([movff s d]))
 (([qw s] retfie)       ([retfie s]))

 (([qw addr] [qw reg] lfsr) ([lfsr reg addr])) ;; macro only
   
 ;; TABLES

 ;; Since this is mostly used for data tables it's probably best to
 ;; let it compile bytes instead of words: always use data word size.
   
 (([db lo] [qw hi] |,|)  ([d2 lo hi]))
 (([qw lo] |,|)          ([db lo]))   

 (([qw w] |,,|)          ([dw w]))

 ;; Number of byte slots available in the current word
 (([db lo] byte-slots)   ([db lo] [qw 1]))
 ((        byte-slots)   ([qw 0]))
   
 ;; CONDITIONALS

 ;; There is a lot of machine support for conditional branching, but
 ;; it is a bit non-orthogonal: there are 2 types of conditionals:

 ;; * btfsc and btfss instructions skip the next instruction based on any bit
 ;; * conditional jumps take the condition from the STATUS register directly

 ;; memory flag
 (([qw f] [qw b] [qw p] bit?) ([bit? f b p]))
 ;; ((bit?)                      (macro: ~bit?)) ;; STUB

 ;; STATUS flag -> conditional jump opcode
 (([qw p] pz?)      ([flag? (asm: bpz) p]))    
 (([qw p] pc?)      ([flag? (asm: bpc) p]))
 (([qw p] pn?)      ([flag? (asm: bpn) p]))


 ;; Conditional skip optimisation for 'then'.
 ;;
 ;; FIXME: not used since the code is not in the current chain.  The
 ;; most important application of this is to optimize "begin
 ;; ... until" spinning loops, which are now directly generated
 ;; properly using a proper definition of "until".
 ;;
 ;;  (([btfsp  p f b a] [bra l1] ,ins [label l2] swapbra)
 ;;   (if (eq? l1 l2)
 ;;       `([btfsp ,(flip p) ,f ,b ,a] ,ins)
 ;;       (error 'then-opti-error)))
 ;;  ((swapbra) ())

 ;; The 'jw/false' macro recombines the pseudo ops from above into
 ;; jump constructs. These use 'r' instructions. (see assembler.ss)
 (([bit?  f b p] [qw l] jw/false)     ([btfsp (flip p) f b 0] [bra l]))

 ; (([flag? opc p] [qw l] jw/false)     ([,opc (begin (display "WARNING:flag") (flip p)) l]))
 (([flag? opc p] [qw l] jw/false)     ([,opc (flip p) l]))
   
 (([cmp? opc f a 0] [qw l] jw/false)  ([,opc f a] [skip] [bra l]))
 (([cmp? opc f a 1] [qw l] jw/false)  ([,opc f a] [bra l]))

 ;; FIXME: using carry is simpler, since it's not affected by 'drop'
 ;; (([qw l] jw/false)       (macro: ~>z ,(insert `([bpz 0 ,l])))) ;; STUB
 (([qw l] jw/false)       ([decf WREG 0 0] [drop] [bpc 1 l]))

 ;; The 'not' macro is useful as predicate negation. Note that it's not the
 ;; same as "FF XOR" !
 (([bit?  f b p] not)     ([bit? f b (flip p)]))
 (([flag? opc p] not)     ([flag? opc (flip p)]))
 (([cmp?  opc f a p] not) ([cmp?  opc f a (flip p)]))

 

 ;; TESTS
 
 ;; Tests that do not consume their arguments: ( a b -- a b ? )

 ;; The polarity bit in the opcode is chosen such that the 'jw/false'
 ;; macro has a simple 'zero?' comparison for inserting the [bra 1] =
 ;; skip instruction.
 
 ((=?) ([cmp? (asm: cpfseq) INDF0 0 1]))
 ((>?) ([cmp? (asm: cpfsgt) INDF0 0 1]))
 ((<?) ([cmp? (asm: cpfslt) INDF0 0 1]))

 ;; Direct bit operations. These to not modify top, so swap
 ;; instructions if there's a drop.
 
 ;; Doesn't work if f is WREG!!!
   
 ;; It is possible to use literal and/or to operate on WREG though, so
 ;; as a general rule, you are not allowed to touch WREG in forth!

 (([drop] [qw f] [qw b] [qw c] bit!) ([bpf (flip c) f b 0] [drop]))
 (([qw f] [qw b] [qw c] bit!)        ([bpf (flip c) f b 0]))

 ;; ((bit!)                             (macro: ~bit!))  ;; STUB
 
 (([qw f] [qw b] toggle) ([btg f b 0]))
 ;; ((toggle)               (macro: ~toggle)) ;; STUB
   


 ;; MISC

 (([qw a] neg) ([qw (tv: a -1 *)]))
 ((neg)        ([negf WREG 0]))

 ;; pre/postincrement variable fetch
 (([movf f 0 0] preinc)  ([incf f 1 0] [movf f 0 0]))
 (([movf f 0 0] postinc) ([movf f 0 0] [incf f 1 0]))
 
 ((umul>PROD) ([mulwf POSTDEC0 0] [drop]))

 ;; unsigned min/max
 ((max)    ([cpfsgt INDF0 0] [movwf INDF0 0] [drop]))
 ((min)    ([cpfslt INDF0 0] [movwf INDF0 0] [drop]))



 ;; The name BADNOP comes from a very early implementation of the
 ;; compiler, which encoded error conditions in #xF000 NOP
 ;; instructions. the generic error was #xFBAD.
 
 ((badnop)  ([_nop #xBAD]))
   

 ;; Save will undo a previous drop, but will be translated to dup
 ;; otherwise.  I.e. it saves the working register, allowing it to be
 ;; overwritten.  ( A drop would indicate that it's contents can be
 ;; clobbered, so no stack ops are necessary. )
 ((save)                    ([save]))
 

 )














;;                     *** COMPOSITE MACROS ***


(compositions
 (macro) macro:


 (address  word-address 2 *)
 
 ;; data stack registers
 (1st   WREG)
 (2nd   INDF0)
 (2nd-  POSTDEC0)

 ;; misc data stack ops
 (@!       movff)
 (nfdrop   2nd- 1st @!)  ;; drop without affecting flags
 (test     #xff and)
 (>flags   test nfdrop)

 (even?  1st 0 low?)
 (odd?   1st 0 high?)

 ;; the other conditions are defined as cmpf macros
 (>=?   <? not)
 (<=?   >? not)
 
 
 ;; (over    over>x x>)

 ;; i completely forgot about this one, till i looked at brood 1 source.
 (pick  neg PLUSW0 movf)
 (over  1 pick)
 (nip   POSTDEC0 movwf)  ;; stores wreg in 2nd as side effect

 ;; bit ops
 (high   1 bit!)
 (low    0 bit!)

 ;; flags
 (clc    STATUS C low)
 (stc    STATUS C high)
 (c@     0 rot<<c)
 (c!     STATUS !)  ;; kills other flags
 (sign>c STATUS movwf STATUS rot<<!)

 (high? 1 bit?)
 (low?  0 bit?)

 (z?  0 pz?)
 (nz? 1 pz?)
 (c?  0 pc?)
 (nc? 1 pc?)
 (n?  0 pn?)
 (nn? 1 pn?)
 
 ;; Hardware return stack.  FIXME: move to "r" representing the
 ;; byte-size auxilary "retain stack" mapped to main memory, and "x"
 ;; the hardware return stack which isn't a portable resource.
 
 
 ;; shift
 (<< clc rot<<c)
 (2/ #x80 + rot>>c #x40 xor)
 (>> clc rot>>c)

 (rot<<c! rlcf  d=reg)
 (rot>>c! rrcf  d=reg)
 (rot<<!  rlncf d=reg)
 (rot>>!  rrncf d=reg)
 
 ;; multiply
 (u*   umul>PROD PRODL @)
 (u**  u* PRODH @)

 ;; The 'x' stack is the PIC18 hardware return stack.  It is 21 bit
 ;; wide, of which we only use the lower 16 bits.

 ;; The 'r' stack is the byte-size "retain" stack defined in pic18-control@


 (xl TOSL)
 (xh TOSH)
 (xu TOSU)  ;; mostly unused.  Staapl only goes to 16 bit address space.

 (xdrop pop)
 
 ;;(>rs    push rsl !)	   ;; these are inefficient
 ;;(rs>    rsl @ pop)
 
 (_>x   push xh ! xl !)   ;; these operate on 2 bytes at a time
 (_x>   xl @ xh @ pop)

 ;; Load the contents of the x stack into the f register. (16 bit)
 ;; Mainly useful for mplementing lookup tables.
 (x>f   xl @ fl !
        xh @ fl !
        xdrop)

 ;; Jump table primitive.  Index the word on the hardware return
 ;; stack, skipping a number of instruction words after the stored
 ;; return point.
 (xskip  rot<< dup xl +! 1 and xh ++!)
 

 ;; indirect memory access
 
 ;; second register is 'a'
 (ah FSR2H)
 (al FSR2L)

 (stack-data-ptr FSR0L)

 ;; the 'f' register is similar but for flash program memory access
 (fu TBLPTRU)
 (fh TBLPTRH)
 (fl TBLPTRL)
 
 ;; using double '!' and '@' to indicate a and p are 2-byte regs
 (a@@   al @ ah @)  ;; ( -- lo hi )
 (f@@   fl @ fh @)  ;; ( -- lo hi )

 (~a!!  ah ! al !)  ;; ( lo hi -- )

 
 (f!!   fh ! fl !)  ;; ( lo hi -- )

 (f!!!  fu ! fh ! fl !)  ;; ( lo hi u -- )
 
 (@a+  POSTINC2 @)
 (!a+  POSTINC2 !)
 
 (@a-  POSTDEC2 @)
 (!a-  POSTDEC2 !)

 (!+a  PREINC2 !)
 (@+a  PREINC2 @)
 
 (@a   INDF2 @)
 (!a   INDF2 !)

 ;; indirect addressing using FSR2 + WREG
 (@i   PLUSW2 movf)
 (!i   POSTDEC0 PLUSW2 movff drop)


 ;; save/drop at beginning/end to enable opti
 (@f+   save tblrd*+ TABLAT movf)
 (@f    save tblrd*  TABLAT movf)
 (!f+   TABLAT movwf tblwt*+ drop)
 
 ;; conditional branching
 ;; (abs      1st 7 high? if neg then)

 ;; standard forth meaning: ( a b -- ? )
 ; (=    xor nfdrop z?)
 ; (>=   - nfdrop c?)
 ; (<    - nfdrop nc?)

;; (~not   z? if -1 else 0 then) ;; FIXME: do this better

   
 

;; In Forth code, org uses byte addresses.
 (code-size  2)  ;; Size of a code cell in bytes.
 )

(patterns
 (pic18-post)

 ;; POST PROCESSING

 ;; There is a single postprocess hook that runs multiple passes over
 ;; the first pass assembly output. These words are specified as
 ;; macros, and executed after pushing an assembly instruction to the
 ;; asm stack.

 ;; The target should leave pseudo ops like QW CW JW and EXIT so
 ;; generic optimizations can be performed on the intermediate asm
 ;; representation produced by the first compilation step. Left over
 ;; pseudo ops are then elimiated using the 'pseudo' macro.
   
 ;; Convert pseudo asm -> real asm
 (([qw a] pseudo)           ([save] [movlw a]))
 (([cw a] pseudo)           ([jsr 0 a]))
 (([jw a] pseudo)           ([jsr 1 a]))
   
 (([movlw a] [exit] pseudo) ([retlw a]))
 (([exit] pseudo)           ([return 0]))

 ((pseudo)                  ())

 ;; 'save' elimination
 (([drop] [save] opti-save) ())
 (([,op (? (tv? POSTDEC0)) 0 0] [save] opti-save) ([,op INDF0 1 0]))
 (([save] opti-save) ([movwf PREINC0 0]))
 ((opti-save) ())

 )

;; Matching target values.
(define (tv? v)
  (let ((_v (target-value-eval v))) ;; this needs to be defined
    (lambda (x)
      (let ((_x (target-value-catch-undefined ;; returns false if we can't eval it
                 (lambda () (target-value-eval x)))))
        ;; (printf "EVALED ~a -> ~a (wanted ~a)\n" x _x _v)
        (equal? _x _v)))))



;; For the PIC18, the code postprocessor is defined in terms of
;; macros.  This way we can reuse the pattern matcher.  However,
;; because these words are not really useful inside code, we don't
;; export them.  We only export the full opti pass as a single
;; function.

;; The function 'macro->postprocess' creates a function that takes a
;; macro operating on a code stack, and folds it over the whole code
;; feeding it one instruction at a time.

(define postproc
  (apply compose
   (map macro->postprocess
        (list (ns (pic18-post) opti-save)   ;; eliminate [save]
              (ns (pic18-post) pseudo)      ;; eliminate other pseudo ops
              ))))

           


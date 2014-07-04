#lang racket/base

(require
 (lib "match.rkt")
 (lib "pretty.rkt")
 "../ns.rkt"    ;; namespaces
;; "../live.rkt"  ;; reflection
 "../asm.rkt"
 "../tools.rkt"

 ;; These need to be here to make sure we don't redefine virtual
 ;; instructions in the concrete assembler.
 
 "../coma/macro.rkt"
 )


(provide (all-defined-out))


;; PIC18 ASSEMBLER

;; the instruction set is stored in the way it appears in the data
;; sheet: the 'instruction-set' macro will map it to a more readable
;; prototype and onwards to assembler and disassembler code for every
;; opcode.

;; f = file register
;; d = 0 for result destination to be WREG register
;; d = 1 for result destination to be file register (f)
;; a = 0 to force Access Bank
;; a = 1 for BSR to select bank

;; lower case letters signify unsigned values, while capital names are
;; interpreted as signed values (by disassembler)

;; R = relative jump address, in words relative to next
;; instruction. these are also checked for overflow.

(instruction-set

 ;; byte-oriented file register operations
 (addwf   (f d a) "0010 01da ffff ffff")
 (addwfc  (f d a) "0010 00da ffff ffff")
 (andwf   (f d a) "0001 01da ffff ffff")
 (comf    (f d a) "0001 11da ffff ffff")
 (rlcf    (f d a) "0011 01da ffff ffff")
 (rlncf   (f d a) "0100 01da ffff ffff")
 (rrcf    (f d a) "0011 00da ffff ffff")
 (rrncf   (f d a) "0100 00da ffff ffff")
 (subfwb  (f d a) "0101 01da ffff ffff")
 (subwf   (f d a) "0101 11da ffff ffff")
 (subwfb  (f d a) "0101 10da ffff ffff")
 (swapf   (f d a) "0011 10da ffff ffff")
 (xorwf   (f d a) "0001 10da ffff ffff")
 (decf    (f d a) "0000 01da ffff ffff")
 (incf    (f d a) "0010 10da ffff ffff")
 (iorwf   (f d a) "0001 00da ffff ffff")
 (movf    (f d a) "0101 00da ffff ffff")

 (incfsz  (f d a) "0011 11da ffff ffff")
 (infsnz  (f d a) "0100 10da ffff ffff")
 (decfsz  (f d a) "0010 11da ffff ffff")
 (decfsnz (f d a) "0100 11da ffff ffff")
 
 (tstfsz  (f a)   "0110 011a ffff ffff")
 (clrf    (f a)   "0110 101a ffff ffff")
 (cpfseq  (f a)   "0110 001a ffff ffff")
 (cpfsgt  (f a)   "0110 010a ffff ffff")
 (movwf   (f a)   "0110 111a ffff ffff") 
 (mulwf   (f a)   "0000 001a ffff ffff")
 (cpfslt  (f a)   "0110 000a ffff ffff")
 (setf    (f a)   "0110 100a ffff ffff")
 (negf    (f a)   "0110 110a ffff ffff")
  
 (movff   (s d)	  "1100 ssss ssss ssss" "1111 dddd dddd dddd")
 

 ;; ORIGINAL
 ;; bit-oriented file register operations
 ;;(bcf     (f b a) "1001 bbba ffff ffff")
 ;;(bsf     (f b a) "1000 bbba ffff ffff")
 ;;(btfsc   (f b a) "1011 bbba ffff ffff")
 ;;(btfss   (f b a) "1010 bbba ffff ffff")
 ;;(btg     (f b a) "0111 bbba ffff ffff")

 ;; POLARIZED
 ;; p = INVERTED bit value : clear = 1, set = 0
 (bpf    (p f b a) "100p bbba ffff ffff")
 (btfsp  (p f b a) "101p bbba ffff ffff")
 (btg    (f b a)   "0111 bbba ffff ffff")

 ;; ORIGINAL
 ;; control operations
 ;;(bc      (n)     "1110 0010 nnnn nnnn")
 ;;(bnc     (n)     "1110 0011 nnnn nnnn")
 ;;(bn      (n)     "1110 0110 nnnn nnnn")
 ;;(bnn     (n)     "1110 0111 nnnn nnnn")
 ;;(bov     (n)     "1110 0100 nnnn nnnn")
 ;;(bnov    (n)     "1110 0101 nnnn nnnn")
 ;;(bz      (n)     "1110 0000 nnnn nnnn")
 ;;(bnz     (n)     "1110 0001 nnnn nnnn")
 
 ;; POLARIZED
 ;; conditionals
 ;; p = polarity   1 : inverted   0 : normal
 (bpc      (p R)     "1110 001p RRRR RRRR")
 (bpn      (p R)     "1110 011p RRRR RRRR")
 (bpov     (p R)     "1110 010p RRRR RRRR")
 (bpz      (p R)     "1110 000p RRRR RRRR")
 
 (bra     (R)     "1101 0RRR RRRR RRRR")
 (_call   (s l h) "1110 110s llll llll" "1111 hhhh hhhh hhhh") ;; h = high, l = low  (~nop h)
 (clrwdt  ()      "0000 0000 0000 0100")
 (daw     ()      "0000 0000 0000 0111")
 (_goto   (l h)   "1110 1111 llll llll" "1111 hhhh hhhh hhhh") ; (~nop h)
 (nop     ()      "0000 0000 0000 0000")
 (_nop    (d)     "1111 dddd dddd dddd") ; used for extra argument
 (pop     ()      "0000 0000 0000 0110")
 (push    ()      "0000 0000 0000 0101")
 (reset   ()      "0000 0000 1111 1111")
 (rcall   (R)     "1101 1RRR RRRR RRRR")
 (retfie  (s)     "0000 0000 0001 000s")
 (retlw   (k)     "0000 1100 kkkk kkkk")
 (return  (s)     "0000 0000 0001 001s")
 (sleep   ()      "0000 0000 0000 0011")

 ;; literal operations
 (addlw   (k)     "0000 1111 kkkk kkkk")
 (andlw   (k)     "0000 1011 kkkk kkkk")
 (iorlw   (k)     "0000 1001 kkkk kkkk")
 (movlb   (k)     "0000 0001 0000 kkkk")
 (movlw   (k)     "0000 1110 kkkk kkkk")
 (mullw   (k)     "0000 1101 kkkk kkkk")
 (sublw   (k)     "0000 1000 kkkk kkkk")
 (xorlw   (k)     "0000 1010 kkkk kkkk")

 ;; data memory <-> program memory operations
 (tblrd*  ()      "0000 0000 0000 1000")
 (tblrd*+ ()      "0000 0000 0000 1001")
 (tblrd*- ()      "0000 0000 0000 1010")
 (tblrd+* ()      "0000 0000 0000 1011")
 (tblwt*  ()      "0000 0000 0000 1100")
 (tblwt*+ ()      "0000 0000 0000 1101")
 (tblwt*- ()      "0000 0000 0000 1110")
 (tblwt+* ()      "0000 0000 0000 1111")

 (_lfsr   (f l h) "1110 1110 00ff hhhh" "1111 0000 llll llll")  ; (~nop l)


 )

(check-set-mode! 'report-failed)
(check ((asm-fn (asm: movlw)) 0 123) => '(3707))

;; (check ((dasm-find 14 8) 3707) => '(movlw (k . 123)))


(define (page addr) (>>> addr 8))

(define-syntax-rule (delegate-asm name) (asm-fn (asm: name)))

(define-lowlevel-ops

 ;; Pseudo primitives.
 ((dup   here)      `(#x6EEC))
 ((drop  here)      `(#x50ED))
 ((db    here x)     `(,(int x)))
;; ((dw    here x)     `(,(int x)))
 ((d2    here lo hi) `(,(bior (int8 lo)
                         (<<< (int8 hi) 8))))

 ;; One-size-fits all branch instruction.
 ((jsr   here exit address) (smart-jsr here exit address))

 ;; Simpler interface to multi-word instructions.
 ((lfsr  here f addr) ((delegate-asm _lfsr) here f addr (page addr)))
 ((goto  here addr)   ((delegate-asm _goto) here addr (page addr)))
 ((call  here addr s) ((delegate-asm _call) here s addr (page addr)))
 ((call0 here addr)   ((delegate-asm _call) here 0 addr (page addr)))) ;; not using shadow




;; The smart unconditional jump / call.
(define (smart-jsr here exit address)
  (let-values
      ;; determine jump/call based on exit flag
      (((short long)
        (if (zero? exit)
            (values (asm: rcall) (asm: call0))
            (values (asm: bra)   (asm: goto)))))

       ;; determine relative/absolute based on distance.
       (let ((jsr (if (relative-ok? here address) short long)))
         ((asm-fn jsr) here address))))


;; Check if we can reach addr from here with a relative jump encoded
;; in 11 bits.
(define (relative-ok? here addr)
  (let ((relative-addr (- addr(+ here 1))))
    (asm-fits? relative-addr
               11
               operand:signed)))

;; asm pretty printer: convert internal asm representation to the
;; canonical one.

;; (define (lfsr reg base bank)
;;  `(lfsr ,reg ,(bior base (<<< bank 8))))


;; (define (pretty-asm lst)
;;   (foldr
;;    (match-lambda*

;;     ((('~lfsr reg (and bank (= number? #t)))
;;       (('~nop (and base (= number? #t))) . r))
;;      (cons (lfsr reg base bank) r))
    
;;     ((('~lfsr reg (and bank (= number? #t)))
;;       (('label . l) ;; HACK
;;        ('~nop (and base (= number? #t))) . r))
;;      (cons (lfsr reg base bank) r))
    
;;     ((('bpf p x y z) r)
;;      (cons `(,(if (= p 0) 'bsf 'bcf) ,x ,y ,z) r))

;;     ((('btfsp p x y z) r)
;;      (cons `(,(if (= p 0) 'btfss 'btfsc) ,x ,y ,z) r))

;;     ((('r 'bpz p addr) r)
;;      (cons `(,(if (= p 0) 'bz 'bnz) ,addr) r))
;;     ((('bpz p addr) r)
;;      (cons `(,(if (= p 0) 'bz 'bnz) ,addr) r))

;;     ((('r 'bpc p addr) r)
;;      (cons `(,(if (= p 0) 'bc 'bnc) ,addr) r))
;;     ((('bpc p addr) r)
;;      (cons `(,(if (= p 0) 'bc 'bnc) ,addr) r))

;;     ((('~movff s) (('~nop d) . r))
;;      (cons `(movff ,s ,d) r))

;;     ((('~nop 'TOSU) r)
;;      (cons '(_) r))
    
;;     ((head tail)
;;      (cons head tail)))
   
;;    '() lst))

;; (pretty-asm '((~lfsr 0 1) (~nop 2) (bpf 0 a b c)))



;; -*- scheme -*-
;; Same representation as Racket staapl.


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
 ;; i = INVERTED bit value
 ;; 0 -> set   / skip if set
 ;; 1 -> clear / skip if clear
 (bsfi   (i f b a) "100i bbba ffff ffff")
 (btfssi (i f b a) "101i bbba ffff ffff")
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
 ;; i = inverted
 (bci      (i R)     "1110 001i RRRR RRRR")
 (bni      (i R)     "1110 011i RRRR RRRR")
 (bovi     (i R)     "1110 010i RRRR RRRR")
 (bzi      (i R)     "1110 000i RRRR RRRR")
 
 (bra     (R)     "1101 0RRR RRRR RRRR")
 (call   (s l h)  "1110 110s llll llll" "1111 hhhh hhhh hhhh") ;; h = high, l = low  (~nop h)
 (clrwdt  ()      "0000 0000 0000 0100")
 (daw     ()      "0000 0000 0000 0111")
 (goto   (l h)    "1110 1111 llll llll" "1111 hhhh hhhh hhhh") ; (~nop h)
 (nop0    ()      "0000 0000 0000 0000")
 (nop    (d)      "1111 dddd dddd dddd") ; used for extra argument
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

 (lfsr    (f l h) "1110 1110 00ff hhhh" "1111 0000 llll llll")  ; (~nop l)

 ;; FIXME: what would it take to support something like this instead,
 ;; where word is concatenated/split for asm/dasm?
 ;; (lfsr    (f (l h)) "1110 1110 00ff hhhh" "1111 0000 llll llll")  ; (~nop l)

 )


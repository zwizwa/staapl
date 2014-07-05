#lang racket/base
(require "../sig.rkt")
(provide (all-defined-out))

;; PIC18 specific wordsets


;; Assembler embedded as macros.
(define-macro-set pic18-assembler^ 
  (movf xorwf andwf iorwf subwf subfwb addwf addwfc comf rrcf rlcf rrncf rlncf
   cpfseq cpfsgt cpfslt clrf setf movwf mulwf
   addlw iorlw xorlw andlw
   push pop sleep reset clrwdt daw tblrd* tblrd*- tblrd*+ tblwt* tblwt*- tblwt*+
   movlw retlw sublw movff retfie lfsr))

;; Machine flags, special operations, and ram/flash index registers.
(define-macro-set pic18-extra^
  (rot<<c rot>>c rot<< rot>> rot>>4
   z? nz? c? nc? n? nn?
   @f+ @f !f+
   @a+ @a @a-
   !a+ !a !a-
   @i !i
   a!! f!! f!!! ah al fh fl

   b@ b! @b !b  ;; banked access

   -- ++ ++! --! ;; use of carry flag is considered low-level
   stc clc
   rot<<c! rot>>c! rot<<! rot>>!
   d=reg
   ",,"

   1st 2nd 2nd-

   ;; non-dropping binary conditionals
   >=? <=? =? >? <? nfdrop
   
   
   ;; hardware return stack
   xl xh xu xdrop _>x _x> x>f xskip

   word-address
   
   ))


(require racket/unit)
(define-syntax-rule (define-macro/id-set macro^ id^ words)
  (begin
    (define-macro-set macro^ words)
    (define-signature id^ words)))

;; This is a minimal set of constants necessary to support the pic18
;; compiler and library.

(define-macro/id-set
  pic18-const^  
  pic18-const-id^
  (
   ;; compiler
   WREG STATUS PLUSW0 PLUSW2 C Z
   PRODL PRODH
   TOSL TOSH TOSU
   FSR0H FSR0L
   FSR1H FSR1L 
   FSR2H FSR2L
   TBLPTRU TBLPTRH TBLPTRL TABLAT
   INDF0 PREINC0 POSTINC0 POSTDEC0
   INDF1 PREINC1 POSTINC1 POSTDEC1
   INDF2 PREINC2 POSTINC2 POSTDEC2
   BSR

   ;; Library
   SPBRG TXSTA RCSTA SPBRGH BAUDCON BRG16 CREN RCIF TRMT OERR TXREG RCREG FERR
   STKPTR
   INTCON GIE PIR1 
   EECON1 EECON2 CFGS EEPGD WREN FREE WR
    
   ))



;; Chip/Board-specific config and init.
(define-macro-set pic18-chip^
  (fosc        ;; oscillator Hz
   init-chip   ;; chip-specific init
   baud        ;; hard-coded monitor baud rate
   init-serial ;; chip-specific serial port init
   ))



;; (require "../rpn/rpn-signature-forms.rkt")
;; (define-signature prefix-test^
;;   (macro/plus
;;    (prefix-parsers
;;     (macro)
;;     ((plus3) (plus plus plus)))))
;; (define-signature abc^ (a b c))
;; (define-signature cba^ (c b a))



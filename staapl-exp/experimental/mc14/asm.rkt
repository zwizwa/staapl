#lang racket/base
(require
 ;; "const.ss"
 "../asm.ss")

;; Mid-Range Flash 8-bit PIC Microcontroller with 14 bit instruction word.
;; Includes PIC12 and PIC16 models.

;; Don't cares (x) set to 0.
(instruction-set

 (addwf  (f d)        "00 0111 dfff ffff")
 (andwf  (f d)        "00 0101 dfff ffff")
 (clrf   (f  )        "00 0001 1fff ffff")
 (clrw   (   )        "00 0001 0000 0000") ;; "00 0001 0xxx xxxx"
 (comf   (f d)        "00 1001 dfff ffff")
 (decf   (f d)        "00 0011 dfff ffff")
 (decfsz (f d)        "00 1011 dfff ffff")
 (incf   (f d)        "00 1010 dfff ffff")
 (incfsz (f d)        "00 1111 dfff ffff")
 (iorwf  (f d)        "00 0100 dfff ffff")
 (movf   (f d)        "00 1000 dfff ffff")
 (movwf  (f  )        "00 0000 1fff ffff")
 (nop    (   )        "00 0000 0000 0000") ;; "00 0000 0xx0 0000"
 (rlf    (f d)        "00 1101 dfff ffff")
 (rrf    (f d)        "00 1100 dfff ffff")
 (subwf  (f d)        "00 0010 dfff ffff")
 (swapf  (f d)        "00 1110 dfff ffff")
 (xorwf  (f d)        "00 0110 dfff ffff")
 (bcf    (f b)        "01 00bb bfff ffff")
 (bsf    (f b)        "01 01bb bfff ffff")
 (btfsc  (f b)        "01 10bb bfff ffff")
 (btfss  (f b)        "01 11bb bfff ffff")
 (addlw  (k  )        "11 1110 kkkk kkkk") ;; "11 111x kkkk kkkk"
 (andlw  (k  )        "11 1001 kkkk kkkk")
 (call   (k  )        "10 0kkk kkkk kkkk")
 (clrwdt (   )        "00 0000 0110 0100")
 (goto   (k  )        "10 1kkk kkkk kkkk")
 (iorlw  (k  )        "11 1000 kkkk kkkk")
 (movlw  (k  )        "11 0000 kkkk kkkk") ;; "11 00xx kkkk kkkk"
 (retfie (   )        "00 0000 0000 1001")
 (retlw  (k  )        "11 0100 kkkk kkkk") ;; "11 01xx kkkk kkkk"
 (return (   )        "00 0000 0000 1000")
 (sleep  (   )        "00 0000 0110 0011")
 (sublw  (k  )        "11 1100 kkkk kkkk") ;; "11 110x kkkk kkkk" 
 (xorlw  (k  )        "11 1010 kkkk kkkk")
)


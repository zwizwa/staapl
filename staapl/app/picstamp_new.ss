#lang scheme/base
(require (planet zwizwa/staapl/pic18)) 

;; Code for the USBPicStamp by Johannes Taelman.
;; http://www.flickr.com/photos/_-j/1350651639/

;; staaplc -u -d pk2 picstamp.f 



(words-org-flat
 (#x300000 ;; Address of configuration bits in code space.

  ;; FIXME: this is code, and not a data table -- add sugar.
  #x21 |,| #x02 |,| #x3A |,| #x1E |,|
  #x00 |,| #x81 |,| #x85 |,| #x00 |,|   ;; extended instruction set disabled
  #x0F |,| #xC0 |,| #x0F |,| #xE0 |,|
  #x0F |,| #x40 |,| ))

(require
 ; target code and macros
 (planet zwizwa/staapl/pic18/shift)
 (planet zwizwa/staapl/pic18/interpreter)
 (planet zwizwa/staapl/pic18/route)
 (planet zwizwa/staapl/pic18/ramblock)
 (planet zwizwa/staapl/pic18/template)
; (planet zwizwa/staapl/pic18/inc/P18F2550)
 )


(macros
 
 (fosc 48000000) ; MHz  ; 4 clock cycles per instruction cycle gives 12 MIPS.
 (baud 230400)

 )


; load monitor-serial.f \ boot block + serial monitor code (UART)

    

;; #sh# pk2cmd -I -M -R -p PIC18F2550 -f $1


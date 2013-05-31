#lang planet zwizwa/staapl/pic18 \ -*- forth -*-
provide-all

\ Macros for serial port init.

macro
: fosc/baud/div>count | fosc baud div |
    fosc    \ Global time base
    div /   \ Divisor: 64, 16 or 4
    baud /  \ Baud rate.
    1 - ;

\ Not all machines have the 16 bit BRG, for example the older 252 and
\ 452 models. for newer (4-digit?) chips the 16bit timer can be used.
\ see serial-16.f

: async.init-brg-8 | baud fosc |
    fosc baud 16 fosc/baud/div>count SPBRG !
    #x24 TXSTA !               \ enable transmission and high baud rate
    #x90 RCSTA ! ;             \ enable serial port and reception    
    

forth
  
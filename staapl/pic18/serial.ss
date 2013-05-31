#lang scheme/base
(require "../pic18.ss")
(provide (all-defined-out))

;; FIXME: experimental s-expression based word/variable defs.


(words
 (async.rx>
    begin async.rx-ready? until
    async.overrun? if
        async.rx-reset
        async.rx> exit
    then
    RCREG @
    async.frame? if
        drop
        async.rx> exit
    then)

 (async.>tx
    begin async.tx-ready? until TXREG !)

 )

(compositions
 (macro) macro:

 (async.rx-reset
    RCSTA CREN low
    RCSTA CREN high)

 (async.overrun?   RCSTA OERR  high?)
 (async.frame?     RCSTA FERR high?)

 (async.rx-ready?  PIR1  RCIF high?)
 (async.tx-ready?  TXSTA TRMT high?)

    
 (async.debug-transmit
    begin
        #xFF
        async.>tx
    again)
 
 (async.debug-loopback
    begin
        async.rx>
        async.>tx
    again)
 
 )


              

#|

#lang planet zwizwa/staapl/pic18 \ -*- forth -*-
provide-all

\ Serial port target code - no interrupts.

\ There are two versions of the serial port hardware.  Older 8-bit
\ version as present in the 3-digit chips, and the newr 16-bit version
\ as present in the 4-digit chips.

\ The newer architecture is backwards compatible with these extra features:
\ - TXSTA SENDB
\ - BAUDCTL
\ - 16bit baud gen (default is 8 bit)
\ - autobaud

\ Note that for high clock rates and low baud rates, this needs a
\ 16bit baud rate generator (or using the 64 division instead of 16)

\ driver code
macro
: async.rx-reset
    RCSTA CREN low
    RCSTA CREN high ;
: async.overrun?   RCSTA OERR high? ;
: async.frame?     RCSTA FERR high? ;
forth


macro  
: async.rx-ready? PIR1  RCIF high? ;
: async.tx-ready? TXSTA TRMT high? ;
forth

: async.rx>
    begin async.rx-ready? until
    async.overrun? if
        async.rx-reset
        async.rx> ;
    then
    RCREG @
    async.frame? if
        drop
        async.rx> ;
    then ;

: async.>tx
    begin async.tx-ready? until TXREG ! ;

macro


\ DEBUG  
  
\ send out all ones, which gives a pattern 0 11111111 1... which can
\ be easily measured with a frequency counter to see if the rate is
\ right. frequency = baud rate / 10
    
: async.debug-transmit
    begin
        #xFF
        async.>tx
    again ;

: async.debug-loopback
    begin
        async.rx>
        async.>tx
    again ;
forth

|#
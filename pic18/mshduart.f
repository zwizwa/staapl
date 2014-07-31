#lang staapl/pic18 \ -*- forth -*-
provide-all
\ master-slave half-duplex uart


staapl pic18/compile-macro



macro
: tris  TRISB 7 ;
: lat   LATB  7 ;
: port  PORTB 7 ;    
: LOW   tris low ;
: HIZ   tris high ;
: bit   out . in . ;
: 4bit  bit bit bit bit ;
: byte  4bit 4bit ;
forth

variable data
: out data 0 high? if HIZ else LOW ;
: in  port high? if clc else stc data rot>>c! ;
        
: init-master
    H #xFF data !
    lat low \ switch between LOW and HIZ
  
\ Reset happens in master-tick  
: state+
    state @ state 1+! ;
: state-reset
    \ save current to output buffer, load new from input
    \ default is #xFF
    #xFF data !
    0 state ! ;    

\ State machine clocked at double the bit rate.  Even phases are
\ writes, odd phases are reads.

\ Imp: it seems that jump tables are still the simplest approach to
\ implementing simple state machines: State acts as instruction
\ pointer.  The bit out / in states are just duplicated 8 times.  Hard
\ to write code that makes up for 16 bytes of "wasted" flash :)
: master-tick
    state+ route
        LOW  nop \ start bit
        byte     \ out, in, repeated 8 times
        HIGH nop \ stop bit
        
        \ fallthrough
        state-reset
        master-tick ;



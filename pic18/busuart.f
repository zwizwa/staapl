\ #lang staapl/pic18 \ -*- forth -*-
\ provide-all

\ master-slave half-duplex uart
\ single line with pullup resistor
\ master provides start bit and transmits 0xFF (all release) as idle / poll
\ slave syncs to start bit and can pull data bits low
\ stata machine, easy to include in real-time system

staapl pic18/compose-macro
staapl pic18/route



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
: out data 0 high? if HIZ else LOW then ;
: in  port   high? if stc else clc then data rot>>c! ;

        
: init-master
    HIZ #xFF data !
    lat low ; \ switch between LOW and HIZ
  

variable state
: state+
    state @ state 1+! ;
: state-reset
    \ save current to output buffer, load new from input
    \ default is #xFF
    #xFF data !
    0 state ! ;    

\ State machine clocked at twice the bit rate.
\ Even phases are writes, odd phases are reads.

\ Imp: it seems that jump tables are still the simplest approach to
\ implementing simple state machines: State acts as instruction
\ pointer.  The bit out / in states are just duplicated 8 times.
    
\ Difference between master and slave is who generates the start bit.
\ Slave will sync on 1->0 edge and clock the machine 20 times.

macro    
: tick | start |
    state+ route
        ' start i nop  \ start bit
        byte           \ out, in, repeated 8 times
        HIZ nop        \ stop bit
        
        \ fallthrough
        state-reset
        tick ;
\ instantiate one of these        
: tick-master ' LOW tick ;        
: tick-slave  ' HIZ tick ;        
forth
      

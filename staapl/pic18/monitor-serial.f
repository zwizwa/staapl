
staapl pic18/boot         \ chip init code
staapl pic18/serial
staapl pic18/prom
staapl pic18/route



\ these need to be procedures so they can be overridden by either
\ procedures or macros. (currently macros cannot be overridden by
\ procedures). FIXME: solve redirect inefficiency in asm optimization.

\ Redefine these when necessary.

macro
: receive       async.rx> ;
: transmit      async.>tx ;

\ Packet boundaries: not used for blocking UART.
: rx-sync  ;
: tx-sync  ;    
: tx-end   ;  
  
: interactive?
    debug-serial? ;

 
: init-comm
    baud fosc init-serial ; \ init serial port RX/TX logic

\ For staaplc
: console-type   ` uart ;
: console-device ` /dev/ttyUSB0 ;
: console-baud   baud ;
    
forth
  

: forward-msg forward-msg-ignore ;

load interpreter.f
load monitor-init.f

\ first app word comes here.
    
    

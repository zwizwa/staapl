\ Staapl monitor on top of a synchronous serial protocol over the ICSP
\ pins, implemented using a PICkit2 programmer/debugger.

staapl pic18/prom
staapl pic18/route

load icsp.f

macro
: receive      icsp-rx ;
: transmit     icsp-tx ;
: rx-sync      icsp-handshake ;
: tx-sync      icsp-handshake ;    
: tx-end       icsp-wait-ack ;

\ Start interpreter if PK2 is connected.    
: interactive? icsp-idata low? ;
: init-comm ;

\ For staaplc
: console-type ` pk2 ;    
    
forth

: forward-msg forward-msg-ignore ;


\ Instantiate the interpreter using the definitions above.
load interpreter.f

\ Enable single-command programming and block clobber inspection to
\ avoid large roundtrip delays.
load fast-prog.f
load fast-chkblk.f


\ Common init-XYZ code and macros for interpreter and boot.
load monitor-init.f    







\ Serial monitor running on a bulk IN/OUT pair  (Linux usbserial)
\ http://www.linuxjournal.com/article/6573

staapl pic18/prom
staapl pic18/route

\ Hardcoded to EP1

macro
: receive  OUT1> ;
: transmit >IN1 ;
: rx-sync  ;
: tx-sync  ;
: tx-end   IN1-flush ;

: interactive? 1 ;
: init-comm init-usb-isr ;

\ for staaplc
: console-type   ` uart ;
: console-device ` /dev/ttyACM0 ;
: console-baud   0 ;
    
forth

  
: forward-msg forward-msg-ignore ;

\  Instantiate the interpreter using the definitions above.
load interpreter.f

\ Enable single-command programming and block clobber inspection to
\ avoid large roundtrip delays.
load fast-prog.f
load fast-chkblk.f


\ Common init-XYZ code and macros for interpreter and boot.
load monitor-init.f    

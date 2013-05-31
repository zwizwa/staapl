\ -*- forth -*-

\ this is an example forth file for the 18f1220 chip with serial
\ connection.

\ config
#x30000000 org

\ extended instruction set, no wdt, no code protect, 
\ disabled extended instruction set: 06 : C5 -> 85

#x21 , #x0E , #x3B , #x1E ,
#x00 , #x81 , #x81 , #x00 ,
#x0F , #xC0 , #x0F , #xE0 ,
#x0F , #x40 , 




\ code
0 org

\ time
48000000 constant fosc
9600     constant baud

\ memory
#x80  constant stack-data     \ hidden behind access bank
#xA0  constant stack-control
#x00  constant allot-ram
#x200 constant allot-flash

\ note this chip is running with relative addressing, so the 'global'
\ space #x00-#x5F is not accessible. the real global variables start
\ at #x60, which leaves only 32 bytes there. this is ok, since
\ relative addressing allows the use of objects, so less global state
\ is necessary. due to this, the stacks are moved to the non-reachable
\ space.

path pic18              \ system path
path prj/forthtv        \ application path

load p18f2550.f         \ chip type, needs to be first file loaded
load monitor-serial.f   \ shared serial monitor code

\ this should reflect system name (for later autoload)
: hello fsymbol ForthTV


\ built in leds   FIXME: need testing
macro

: leds  LATB ;
: spin  0 for 0 for next next ;
: red   LATB 0 ;
: green LATB 1 ;
: blue  LATB 2 ;
: blink 0x7 leds ! spin 0 leds ! ;

forth


\ : init-ram 0 0 a!! 8 for 0 for 0 !a+ next next ;
    
    
: warm
    init-stacks     \ setup DS RS XS
 \   init-ram        \ clear ram memory
    init-chip       \ setup outputs and clock
    init-serial     \ init serial port RX/TX logic

\   debug-transmit

    interpreter ;   \ fall into interpreter



allot-flash org

    

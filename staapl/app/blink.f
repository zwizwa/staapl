\ Demo for PIC DIP40 board.
\ load ./blink.f

\ busy loop
: wait0 100 for next ;
: wait1 100 for wait0 next ;
: nwait     for wait1 next ;


: init-led #xF0 TRISB ! ; \ Config port B pins 0 to 3 as outputs
: led #xFF xor LATB ! ;   \ Set LED state for 4 pins at once, active low

: blink for
        #x0 led 100 nwait
        #xF led 100 nwait
    next ;

: left  dup led << 30 nwait ;
: right dup led >> 30 nwait ;

: toaster for 1
        3 for left  next
        4 for right next
        0 led 200 nwait
        next ;
    
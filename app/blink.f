\ Demo for PIC DIP40 board.
\ load ./blink.f

\ busy loop
\ : wait0 100 for next ;
\ : wait1 100 for wait0 next ;
\ : nwait     for wait1 next ;


\ code-efficient busy loop

: pause 0 begin 0 begin 1+ c? until drop 1+ c? until drop ;

\ : pause 0 0 begin 1+ 0 2nd ++! until drop drop ;
    

    
: init-led #xF0 TRISB ! ; \ Config port B pins 0 to 3 as outputs
: led #xFF xor LATB ! ;   \ Set LED state for 4 pins at once, active low

: blink for
        #xF led pause
        #x0 led pause
    next ;

: left  dup led << pause ;
: right dup led >> pause ;

: toaster for 1
        3 for left  next
        4 for right next
        0 led pause
        next ;
    
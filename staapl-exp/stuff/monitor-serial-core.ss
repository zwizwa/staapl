#lang staapl/pic18-unit \ -*- forth -*-

{ require staapl/pic18/sig }
{ import pic18-chip^ }

staapl pic18/boot         \ chip init code
staapl pic18/interpreter  \ command interpreter
staapl pic18/vector
staapl pic18/serial




\ these need to be procedures so they can be overridden by either
\ procedures or macros. (currently macros cannot be overridden by
\ procedures). FIXME: solve redirect inefficiency in asm optimization.

\ Redefine these when necessary.

macro

\ stacks grow upward  
: ds-bottom #x80 ; 
: rs-bottom #x90 ;
    
: init-all-but-xs
    ds-bottom init-ds  \ data stack
    rs-bottom init-rs  \ retain stack
    init-xs       \ hardware control stack
    init-chip ;   \ setup outputs and clock

: init-all    
    init-xs           \ setup XS needs to be a macro
    init-all-but-xs   \ can be shared for alternative reset vector
    baud fosc init-serial \ init serial port RX/TX logic
    use-serial
    ;  

forth

\ Connect serial port to standard io.
: use-serial-in  v-rx -> async.rx> ;
: use-serial-out v-tx -> async.>tx ;    
: use-serial
    use-serial-in
    use-serial-out ;

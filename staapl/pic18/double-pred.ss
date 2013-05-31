#lang planet zwizwa/staapl/pic18 \ -*- forth -*-
staapl pic18/double-math
provide-all

\ predicates

: _=
    _-
: _0=    
    INDF0 iorwf
    z? if
        INDF0 setf
        -1 retlw
    then
        INDF0 clrf
        0 retlw
    
\ SIGNED

: _sign>flag-invert
    #x80 xor
: _sign>flag
    rot<< 1 and
    0 sublw
    INDF0 movwf ;

    
: _<   _-   _sign>flag ;
: _>=  _-   _sign>flag-invert ;

: _>   _-   _negate _sign>flag ;
: _<=  _-   _negate _sign>flag-invert ;
    

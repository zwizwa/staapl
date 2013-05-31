#lang planet zwizwa/staapl/pic18 \ -*- forth -*-
provide-all

\ Template buffer for initializing RAM.  Returns the address of the
\ FLASH table on the stack.
    
: template \ -- lo hi
    TOSL @
    TOSH @
    pop ;
    
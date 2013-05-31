#lang staapl/pic18 \ -*- forth -*-
provide-all

\ Partition RAM in blocks of 16 bytes.  This makes it possible to
\ access it using a one byte address.

macro
: >block 16 / ;  
forth
: block>a \ blockn --
    swap-nibble dup #x0F and ah ! #xF0 and al ! ;  
: clear-block \ blockn --
    block>a 16 for 0 !a+ next ;

\ Initialize a RAM block from Flash ROM memory.
: init-block  \ addrl addrh blockn --
    block>a
    fh ! fl ! 
    16 for @f+ !a+ next ;
    

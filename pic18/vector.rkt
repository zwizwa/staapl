#lang staapl/pic18 \ -*- forth -*-

staapl pic18/execute
staapl pic18/afregs
provide-all


\ the vector representation used BYTE ADDRESSES, however, this is
\ abstract.

\ perform execution given a variable address: extra level of indirection
macro    
: 2@ dup >m @ m> 1 + @ ;
: 2! dup >m 1 + ! m> ! ;    
: invoke 2@ execute/b ;
: ->  do-arrow reachable ;    
forth

\ implementing this word as a macro leads to too much code, so i'm
\ using indirect addressing. 
: do-arrow
    a>r
    0 a!!
    TOSL @ !a+
    TOSH @ !a+
    r>a
    pop ;



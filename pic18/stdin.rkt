#lang staapl/pic18 \ -*- forth -*-
provide-all
staapl pic18/vector

2variable stdin
: i> stdin invoke ;

\ These are small and often used.
: i=a  stdin -> @a+ ;
: i=f  stdin -> @f+ ;    
: i=d  stdin -> ;        \ data stack

\ Save/restore stdin

: i[
    stdin @ >r
    stdin 1 + @ >r ;

: ]i
    r> stdin 1 + !
    r> stdin ! ;

    
    

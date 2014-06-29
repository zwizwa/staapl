#lang staapl/pic18 \ -*- forth -*-
provide-all
staapl pic18/vector

2variable stdin
: i> stdin invoke ;

\ These are small and often used.
: a>i  stdin -> @a+ ;
: f>i  stdin -> @f+ ;    
: d>i  stdin -> ;        \ data stack

    \ Save/restore stding

: stdin>r
    stdin @ >r
    stdin 1 + @ >r ;

: r>stdin
    r> stdin 1 + !
    r> stdin ! ;

    
    

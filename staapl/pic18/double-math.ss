#lang staapl/pic18 \ -*- forth -*-
provide-all

\ double word math 8 -> 16 bit


\ some 16bit math routines. i don't have time for it now, but at this
\ point, a 16 bit forth would come in handy. all math on periods is
\ unsigned.
    
: _dup    over over ;

: _2drop _drop
: _drop  drop drop ;
    
: _nip    >r >r drop drop r> r> ;    
: _<<     clc 2nd rot<<c! rot<<c ;
: _>>     >> 2nd rot>>c! ;        \ unsigned
: _2/     2/ 2nd rot>>c! ;        \ signed

: _+               \ ( a b c d ) ( )
    >r swap>r      \ ( a c )     ( b d )
    + r> r- @ ++ ;

: _-
    >r swap>r      \ ( a c )     ( b d )
    - r> r- @ -- ;

: _invert
    WREG comf d=reg
    2nd comf d=reg ;


\ these leave carry flag intact, but not the zero flag
: _1+
    dup >r
    1 movlw INDF0 addwf  d=reg
    0 movlw INDF1 addwfc d=reg
    drop r> ;
    
: _1-
    dup >r
    -1 movlw INDF0 addwf  d=reg
             INDF1 addwfc d=reg
    drop r> ;
    


: _negate
    _invert
    _1+ ;

    
\ revised routines

: 2hi>r >r swap>r ;
: _and  2hi>r and r> r> and ;
: _xor  2hi>r xor r> r> xor ;
: _or   2hi>r or  r> r> or ;


\ combine low parts of 16 bit words into one 16bit word
: _lohi >r drop drop r> ;    

macro
\ : lohi | val | val val 8 >>> ;  \ defined in execute.f
\ : 2@   dup >m @ m> 1 + @ ; \ addr -- lo hi  \ defined in vector.f
\ : 2!   dup >m 1 + ! m> ! ; \ lo hi addr --   
forth


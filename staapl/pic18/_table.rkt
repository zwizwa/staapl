#lang staapl/pic18 \ -*- forth -*-
provide-all

\ double lookup table: works like 'route' but returns 2 bytes.

: _table       \ index
    rot<<      \ word addressing (hide MSB in LSB)
    dup
    TOSL +!      \ add offset to return address
    1 and      \ recover high bit
    TOSH ++!     \ add carry flag and
    
    TOSL 0 low   \ clear the hidden MSB
    
    TOSL @ fl ! 
    TOSH @ fh !

    pop
    @f+ @f+ ;

    

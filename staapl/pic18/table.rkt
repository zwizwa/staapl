#lang staapl/pic18 \ -*- forth -*-
provide-all
\ Byte lookup table: works like 'route' but returns a byte.

: table       \ index
    TOSL +!     \ add offset to return address
    0 TOSH ++!  \ add carry flag
    
    TOSL @ fl !  \ r>f  NOTE: this messes up the VM
    TOSH @ fh !

    pop
    @f+ ;

macro    
: table-begin begin ;
: table-end \ -- table-size
    byte-slots >m m-swap begin m> m> - 2 * m> - ;
forth
      
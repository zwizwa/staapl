#lang planet zwizwa/staapl/pic18
provide-all
\ Byte lookup table: works like 'route' but returns a byte.

: table       \ index
    TOSL +!     \ add offset to return address
    0 TOSH ++!  \ add carry flag
    
    TOSL @ fl !  \ r>f  NOTE: this messes up the VM
    TOSH @ fh !

    pop
    @f+ ;


macro
: rs! movlw STKPTR movwf ;
: ds! 1 - 0 lfsr ;
: xs! 1 - 1 lfsr ;

\ Should work for all 18F devices
\ Stacks grow from low to high addresses

\ Note that in "clean" Forth code, the data stack will never grow very
\ large.  However the retain stack might get large depending on depth
\ of context (e.g. shallow / dynamic binding of variables).

: init-stacks-simple
    0 rs!       \ hardware return stack
    #x80 ds!    \ data stack 80-8F 32 bytes
    #xA0 xs!    \ aux stack  90-FF 96 bytes
;    
    
forth
  
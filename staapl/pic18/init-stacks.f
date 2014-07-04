macro
: rs! movlw STKPTR movwf ;
: ds! 1 - 0 lfsr ;
: xs! 1 - 1 lfsr ;

\ Should work for all 18F devices    
: init-stacks-simple
    0 rs!       \ hardware return stack
    #x80 ds!    \ data stack
    #xC0 xs!    \ aux stack
;    
    
forth
  
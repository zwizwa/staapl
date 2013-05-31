#lang staapl/pic18
\ some shift words
provide-all

: >>3    #xF8 and
: rot>>3 rot>>
: rot>>2 rot>> rot>> ;
: >>2    #xFC and rot>>2 ;
: >>4    swap-nibble #xF and ;

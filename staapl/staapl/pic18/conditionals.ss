#lang planet zwizwa/staapl/pic18 \ -*- forth -*-
provide-all

\ Standard forth conditionals not using the flags.  These are less
\ efficient than the flag macros, but easier to use.  Best to use
\ these if code is not time critical.

macro
: ~cmp  | y n ins |
    POSTDEC0 ins i
    n retlw
    y retlw ;
forth  

: =  -1  0 ' cpfseq ~cmp
: >  -1  0 ' cpfsgt ~cmp
: <  -1  0 ' cpfslt ~cmp
: <=  0 -1 ' cpfsgt ~cmp
: >=  0 -1 ' cpfslt ~cmp
    

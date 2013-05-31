#lang staapl/pic18 \ -*- forth -*-
provide-all

\ Convention is that contents of a & f registers should be saved It's
\ too convenient to have them point at current objects to limit code
\ size and increase speed for words that don't need to be too abstract.

\ Since the retain stack (r) is not the execution stack (x), the
\ following words don't need to be macros.

: a>r al @ >r ah @ >r ;
: r>a r> ah ! r> al ! ;

: f>r fl @ >r fh @ >r ;
: r>f r> fh ! r> fl ! ;

: af>r a>r f>r ;
: r>af r>f r>a ;
    

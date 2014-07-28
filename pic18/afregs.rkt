#lang staapl/pic18 \ -*- forth -*-
provide-all

\ The 'a' pointer reg is used as "current RAM data stream", either for
\ in or output.  The 'f' pointer register is used for current,
\ input-only, "Flash data stream".
macro
: a> @a+ ;
: >a !a+ ;
: f> @f+ ;
forth

\ Convention is that contents of a & f registers should be saved It's
\ too convenient to have them point at current objects to limit code
\ size and increase speed for words that don't need to be too abstract.

\ Since the retain stack (r) is not the execution stack (x), the
\ following words don't need to be macros.

: a[ al @ >r ah @ >r ;
: ]a r> ah ! r> al ! ;

: f[ fl @ >r fh @ >r ;
: ]f r> fh ! r> fl ! ;

: af[ a[ f[ ;
: ]af ]f ]a ;
    
  

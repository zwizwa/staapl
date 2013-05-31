#lang planet zwizwa/staapl/pic18 \ -*- forth -*-
provide-all
staapl pic18/double-math

\ single byte fractional increment
: 00_+     swap>r + #x00 r- @ ++ ;
: ff_+     swap>r + #xFF r- @ ++ ;
: ss_+     WREG 7 high? if ff_+ ; then 00_+ ;  \ signed version
    
: _saturate+  drop drop -1 -1 ;
: _saturate-  drop drop 0 0 ;


\   fractional multiplication:  b a z -- x y
\   computes:  0.ab  1.0z *
\              0.ab  0.Fz *

\   factored into  0.ab 0.0z *  and an addition.
\   observe that the result is only 8 bits: [az]_h, which is
\   augmented by the carry bit of [az]_l + [bz]_h + #x80

\   last term is for proper rounding. note that instead of
\   adding #x80, it's possible to add a uniformly distributed
\   pseudorandom number with average #x80 to get better
\   average behaviour.

    
: 0.0z* \ b a z -- r
    POSTDEC0 mulwf  \ compute az  \ b z
    PRODH +r movff
    PRODL +r movff                  \ b z       \ az_h az_l
    POSTDEC0 mulwf drop             \           \ az_h az_l
    PRODH @ r- @ +                  \ bz_h+az_l \ az_h
    rot<< 1 and                      \ roundbit  \ az_h
    r- @ ++                         \ r         \
;


: 0.0pole+ \ b a z -- b+ a+
    _dup 0.0z* 00_+ ;
: 0.0pole- \ b a z -- b- a-
    _dup 0.0z* neg ff_+ ;


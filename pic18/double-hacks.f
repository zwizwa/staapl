\ -*- forth -*-
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
    POSTDEC0 mulwf    \ a*z         \ b z
    PRODH +r movff
    PRODL +r movff                  \ b z       \ az_h az_l
    POSTDEC0 mulwf drop             \           \ az_h az_l
    PRODH @ r- @ +                  \ bz_h+az_l \ az_h
    rot<< 1 and                     \ roundbit  \ az_h
    r- @ ++                         \ r         \
;


: 0.0pole+ \ b a z -- b+ a+
    _dup 0.0z* 00_+ ;
: 0.0pole- \ b a z -- b- a-
    _dup 0.0z* neg ff_+ ;

\ Above, the multiplier is very close to 1.  It's possible to go up to
\ 2 or 1/2 in order to guarantee the highest 1 bit won't move more
\ than one bit position.  This makes it possible to maintain a
\ floating point representation using a single mantissa shift
\ right(left) / exponent increment(decrement).


\ Sacrifice 2 bits precision and keep mantissa represented as:
\    01hh hhhh llll llll
\ This allows recovery from overflow.
    
\ Adjust such that MSB in mantissa is 0   : 1x.. -> 01x..
: normalize-over \ e ml mh -- e' ml' mh'  
    1st 7 low? if ; then \ already good
    clc rot>>c >r        \ 2/ high byte
    rot>>c               \ 2/ low byte
    2nd 1+!              \ increment exponent
    r> ;

\ Adjust such that MSB-1 in mantissa is 1 : 001x.. -> 01x..
\ Assumes MSB is 0
: normalize-under
    1st 6 high? if ; then \ already good
    >r 2nd 1-!            \ decrement exponent
    rot<<c r>             \ 2* low byte
    rot<<c ;              \ 2* high byte


\ Why floating point?  Glad you asked!  In the synth, some control
\ parameters are exponential.  I.e. our perception corresponds
\ somewhat to the logarithm of parameter magnitude.  This is the case
\ both for signal frequency and for signal amplitude, however it is
\ only used for frequency as other exponential conversion is done
\ using BJT transfer characteristics.

\ The ad-hock floating point numbers combined with ad-hoc limited
\ geometric increment/decrement are enough to construct exponential
\ curves without getting into bit depth issues.


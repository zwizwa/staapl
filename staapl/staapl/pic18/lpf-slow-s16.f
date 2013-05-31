\ 16 bit 1-pole lowpass filter. implements:
\
\   x += a (u - x)
\
\ where a < 2^(-8). more specificly a = .00AA (2 HEX digits)
\
\ internally, the filter has 24 bit precision to prevent roundoff
\ errors, and one byte containing an unsigned filter coeff.

\ suppose the state X is in 8.16 fixed point. this gives the values:
\ x(z) = x0 + x1 z^-1 + x2^z-2
\ u(z) = u0 + u1 z^-1 + u2^z-2 
\ d(z) = d0 + d1 z^-1           = u(z) - x(z)
\ a(z) = a0 z^-2

\ from this follows that the low byte of d(z)a(z) can be dropped (or
\ could be used to round?) and the rest is straighforward.

\ sign correction is for (d0 + z) + d1 z^(-1), which is z * a0^z-2 =
\ a0 * z^-1


\ temp vars for all filters: use vars used by multiplier.
macro
: D0 X0 ;  \ difference (u - x)
: D1 X1 ;    

: lpf/int-common | state |    
    \ perform unsigned8 * unsigned16 multiplication + accumulation
    D0 @ state 3 + mulwf drop
    PRODL rot<<c!              \ get carry bit for rounding
    PRODH @ state 0 + ++!
          0 state 1 + ++!
          0 state 2 + ++!
         
    D1 @ state 3 + mulwf drop
    PRODL @ state 0 + +!
    PRODH @ state 1 + ++!
          0 state 2 + ++!

    \ perform signed correction
    D1 7 high? if
        state 3 + @ state 1 + -!
                  0 state 2 + --!
    then ;

\ ul uh state --
: lpf-tick | state |

    \ compute difference
    D1 ! D0 !
    state 1 + @ D0 -!
    state 2 + @ D1 --!

    state lpf/int-common ;

\ same as above, but as integrator: no state decay.
    
\ ul uh state --
: int-tick | state |

    \ store input
    D1 ! D0 !
    state lpf/int-common ;

    
: lpf-out | state |
    \ get value
    state 1 + @
    state 2 + @ ;

\ coef state --
: lpf-init | state |
    state 3 + !       \ save coef
    0 state !         \ zero state vector
    0 state 1 + !
    0 state 2 + ! ;
forth

\   2variable lp
\   2variable lp__
  
\ : _filter-init  #x80 lp lpf-init ;
\ : _filter-tick  lp lpf-tick ;
\ : _filter-out   lp lpf-out ;
    
\ : _filter-0ntick
\     for 0 0 _filter-tick next ;
    
    
    
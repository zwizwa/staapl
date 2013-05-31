\ 16 bit 1-pole lowpass filter. implements:
\
\   x += a (u - x)
\
\ internally, the filter has 24 bit precision to prevent roundoff
\ errors, and one byte containing an unsigned filter coeff.

\ suppose the state S is in 8.16 fixed point. this gives the values:
\ s(z) = s0 + s1 z^-1 + s2^z-2
\ u(z) = u0 + u1 z^-1 + u2^z-2 
\ d(z) = d0 + d1 z^-1           = u(z) - x(z)
\ a(z) = a1 z^-1 + a2 z^-2

\ from this follows that the low byte of d(z)a(z) can be dropped (or
\ could be used to round?) and the rest is straighforward.

\ sign correction is for (d0 + z) + d1 z^(-1), which is

\ z * (a1 z^-1 + a2 z^-2) = a1 + a2 * z^-1

\ temp vars for all filters: use vars used by multiplier.
\ note that the indices correspond to NEGATIVE z powers.

macro
: D0 X0 ;  \ difference (u - x)
: D1 X1 ;    

: A2 3 + ;
: A1 4 + ;    

: S2 0 + ;    
: S1 1 + ;
: S0 2 + ;
    
: lpf/int-common | state |    
    \ perform unsigned8 * unsigned16 multiplication + accumulation
    D1 @ state A2 mulwf drop
    PRODL rot<<c!              \ get carry bit for rounding
    PRODH @ state S2 ++!
          0 state S1 ++!
          0 state S0 ++!
         
    D0 @ state A2 mulwf drop
    PRODL @ state S2 +!
    PRODH @ state S1 ++!
          0 state S0 ++!

    D1 @ state A1 mulwf drop
    PRODL @ state S2 +!
    PRODH @ state S1 ++!
          0 state S0 ++!
    
    D0 @ state A1 mulwf drop
    PRODL @ state S1 +!
    PRODH @ state S0 ++!

    \ perform signed correction: has got 100 * 0.A1A2 = A1.A2 too much.
    D0 7 high? if
        state A2 @ state S1 -!
        state A1 @ state S0 --!
    then ;

\ ul uh state --
: lpf-tick | state |

    \ compute difference
    D0 ! D1 !
    state S1 @ D1 -!
    state S0 @ D0 --!

    state lpf/int-common ;

\ same as above, but as integrator: no state decay.
    
\ ul uh state --
: int-tick | state |

    \ store input
    D0 ! D1 !
    state lpf/int-common ;

    
: lpf-out | state |
    \ get value
    state S1 @
    state S0 @ ;

\ coefl coefh state --
: lpf-init | state |
    state A1 !
    state A2 !       \ save coef
    0 state S2 !     \ zero state vector
    0 state S1 !
    0 state S0 ! ;
forth

\   2variable lp
\   2variable lp__
  
\ : _filter-init  #x80 lp lpf-init ;
\ : _filter-tick  lp lpf-tick ;
\ : _filter-out   lp lpf-out ;
    
\ : _filter-0ntick
\     for 0 0 _filter-tick next ;
    
    
    
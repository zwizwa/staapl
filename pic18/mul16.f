\ core multiplier routine, it computes:
\ (X0 + X1 z) (Y0 + Y1 z) = R0 + R1 z + R2 z^2 + R3 z^3
\ with z = 2^8

\ the names can be bound to global variables, or local variables if
\ the extended instruction set is used.


\ the routine is almost straight from the data sheet DS39605C p72

macro

: Y! Y1 ! Y0 ! ;
: X! X1 ! X0 ! ;

: X@ X0 @ X1 @ ;
: Y@ Y0 @ Y1 @ ;
    
: RL@ R0 @ R1 @ ;
: RH@ R2 @ R3 @ ;
  
: mulcross    
    PRODL @ R1 +!
    PRODH @ R2 ++!
    0 R3 ++! ;


forth

: umul 

    X0 @ Y0 mulwf        \ low terms
    PRODL R0 movff
    PRODH R1 movff drop

    X1 @ Y1 mulwf        \ high terms
    PRODL R2 movff
    PRODH R3 movff drop

    X1 @ Y0 mulwf drop mulcross \ cross terms
    X0 @ Y1 mulwf drop mulcross
    ;
    

\ signed multiplication.

\ suppose X1 is negative. in that case, what we're computing using the
\ unsigned multiplication is actually

\   (X0 + (z + X1) z) (Y0 + Y1 z) 

\ this has a term z^2 (Y0 + Y1 z) too much in the result, which needs to
\ be subtracted. same goes for Y1 being negative.
    
    
\ X: signed
\ Y: unsigned    
: sumul
    umul
    X1 7 high? if
	Y0 @ R2 -!
        Y1 @ R3 --!
    then ;

: smul
    sumul
    Y1 7 high? if
	X0 @ R2 -!
        X1 @ R3 --!
    then ;

    
: R-saturate-signed 
    R3 @ \ save highest byte

    \ get sign bit of low word (+) -> 0, (-) -> 1
    R1 @ rot<< 1 and 

    \ add it to upper word

    \ using machine language here because 'drop' influences the zero
    \ flag, and it's a bit hard to juggle in forth.

    R2 addwf d=reg z? if
        0 movlw
        R3 addwfc d=reg z? if
            \ now R3:R2 contains zero -> no saturation
            R0 INDF0 movff
            R1 movf ;
        then
    then

    \ drop WREG scratch reg, and get saved highest byte
    drop
    
    \ otherwise return maximum int from sign bit
    rot<<c c? if
        drop #x00 #x80 ;
    then
        drop #xFF #x7F ;


    
    
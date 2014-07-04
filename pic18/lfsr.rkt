#lang staapl/pic18 \ -*- forth -*-
provide-all

\ noise generators using polynomials in GF[2] modulo a prime P[x]

\   <n2>:<n1> contains coefficients of the current polynomial n[x]
\ 1:<c2>:<c1> are the coefficients of polynomial P[x]

\ the noise generator calculates n[x] -> x n[x] mod P[x], by performing
\ a shift, followed by a conditional XOR which performs the modulo.

\ setting the lower bits of c1 to zero allows the use of different
\ degrees then 16, giving GF[2^n] for all n <= 24

variable n0 variable n1 \ state
variable c0 variable c1 \ polynomial coefficients

macro

\ shift the register to carry
: rand-shift
    clc n0 rot<<c!
        n1 rot<<c! ;

\ perform modulo operation
: rand-fold
    c0 @ n0 xor!
    c1 @ n1 xor! ;

: rand>c
    rand-shift c? if rand-fold then ;

: init-rand
    \ init noise state: this the primitive polynomial mathematica
    \ gives for field = GF[65536]
    #xD7 c0 ! -1 n0 !
    #x50 c1 ! -1 n1 ! ;

\ fixed 8 bit rng TOS state update, constant duration == 3 cycles this
\ shaves off one cycle by using rotate, and cancelling out the bit if
\ it's one. the 0x1C below is from the polynomial 0x11D.

: rng<  rot<< odd? if #x1C xor then ;

    
forth

: randbyte 0 8                      \ get 8 fresh random bits
: randbits for rand>c rot<<c next  ; \ accum bits -- accum    
: fastrand 0 1 randbits drop n1 @ ; \ take the top of the state bits as random

\ UNSIGNED 8 BIT -> 24 BIT REAL x COMPLEX MULTIPLY ACCUMULATE

\ This implements the core loop and offset correction routines for an
\ inner product using unsigned multiplication.

\ The 'mul/acc' routine reads the next complex filter coefficients (2
\ unsigned bytes) from flash memory, pointed to by the 'f' register.

macro

\ Note it is possible for the accumulator to overflow if the number of
\ taps exceeds 256 (one byte headroom). This is not a problem as long
\ as the eventual accumulation result fits in the 24 bit word length.
  
\ acc --  
: PROD-accumulate | acc |
    PRODL @ acc +!
    PRODH @ acc 1 + ++!
          0 acc 2 + ++!
;

\ Read the next filter coefficient
: coef> @f+ ;
    
\ value acc --
: mac-u8xu8-24 | acc |
    coef> umul>PROD
    acc PROD-accumulate ;

\ value acc --
: mac-dc-16 | acc |
      acc     +!
    0 acc 1 + ++! ;
    
\ Accumulator offset correction for 8 bit FIR coefficients centered at
\ #x80 (flipped sign bit). The most straightforward way is to shift
\ the complex accumulator left one bit, which sets center at #x100, a
\ byte border.

\ acc --
: 3<<! | acc |
    clc acc     rot<<c!
        acc 1 + rot<<c!
        acc 2 + rot<<c! ;

\ DC compensation: subtract a 2 byte value from the 3 byte accumulator.
: -dc! | acc dc |
    dc     @  acc 1 +  -!
    dc 1 + @  acc 2 +  --! ;

\ This converts the contents of the accumulator to a signed value,
\ using the dc accumulator. Note the accumulator has been shifted left
\ one bit.
    
: acc-correct | acc dc |
    acc 3<<!
    acc dc -dc! ;


: 2clear | acc |
    0 acc !
    0 acc 1 + ! ;

: 3clear | acc |
    acc 2clear
    0 acc 2 + ! ;
    
forth  


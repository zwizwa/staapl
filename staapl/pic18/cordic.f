\ CORDIC

\ The core routines in cordic are conditional addition and subtraction
\ of two registers A and B.


\ CORDIC needs a barrel shifter. It's probably easiest to implement
\ this using the multiplier. The shifts happen one after the other, so
\ the shift count can be stored in a register.

\ To implement a cartesian->polar conversion a table of angles is
\ necessary. Then rotation is done iteratively, conditionally on the
\ sign of the Q, while a tan(a) a = s2^(-n) increment will be added to
\ an angle accumulator. Note that the first comparison is on the sign
\ of I.

variable shift-scale

  
2variable I  2variable Is
2variable Q  2variable Qs  


\ Use current shift to fill Is and Qs with shifted versions.  
: cordic-shift
  

: bits \ n --
    table
    128 , 64 , 32 , 16 ,
      8 ,  4 ,  2 ,  1 ,

: shift-right   \ byte bits --
    shift-scale 
    
    




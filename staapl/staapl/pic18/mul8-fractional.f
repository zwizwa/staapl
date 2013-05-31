\ 8x8 multiplication


\ UNSIGNED
\ rounds down
: u.* umul>PROD PRODH @ ;


\ SIGNED

\ Using the same approach for signed as 16x16: i can't figure out how
\ to do separate sign + fraction fast enough to compete with
\ conditional subtraction of cross terms. However, no extra memory
\ required.

\ Suppose X is negative. in that case what we're computing using the
\ unsigned multiplication is actually
\
\   (X + z) Y
\
\ this has a term z too much in the result, which needs to be
\ subtracted. same goes for Y being negative.

\ Core routine that can be reused for mixer core routine    

macro
: s8x8->16/PROD \ a b --
    2nd mulwf
    2nd 7 high?     if dup PRODH -! then
    rot<<c drop     \ sign is now in carry flag
    STATUS 0 high?  if dup PRODH -! then
    drop
    ;
forth  
    
\ Signed fractional multiplication. This uses s1.6 format, so 1 can be
\ represented. In s.7 the 1 will overflow to -1.
  
: .*
    s8x8->16/PROD
    save           \ undo drop
    
    PRODL rot<<c!
    PRODH rot<<c!
    
    PRODL rot<<c!
    PRODH rot<<c! d=w ;

\ Using this to generate exponentials: be careful about rounding
\ errors. It might b e wise to use a version that rounds towards zero
\ instead of down.
    


\ signed multiplication (X,Y) -> XY



\ using conditional subtraction:



\ signed fractional. this is the same as signed, but followed by a
\ shift.


: sat-16->8
    >x              \ save high byte
    dup rot<< 1 and \ get sign bit
    x@ +            \ add high byte (dup to prevent drop)
    z? if
        xdrop drop ;
    then
    drop x>
    rot<<c drop
    c? if
        drop #x80 ;
    then
        drop #x7F ; 
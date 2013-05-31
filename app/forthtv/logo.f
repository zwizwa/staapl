
\ draw a big pixel
: X     WHITE 1_usec ;
: _     BLACK 1_usec ;    

: __    BLACK 20 _usec ;    

\ total number of lines = 250    
\ each pixel is 10 lines high, so it needs to be drawn 10 times, hence
\ the for .. next loop.

\ once the electron gun is off ( _ ) the rest of the line doesn't need
\ to be drawn.

    


: screen-logo
    screen ->

    100 blanks

    10 for hsync __ X X X X X X X X X X _   next
    10 for hsync __ X _                     next
    10 for hsync __ X _ _ X X X _ _ _ X _   next
    10 for hsync __ X X _ _ X _ X _ X _     next
    10 for hsync __ X _ _ _ X _ _ X _       next
    
    100 blanks

    ;
    
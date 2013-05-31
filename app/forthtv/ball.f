\ since this chip is so small and doesn't have a lot of fast math, we
\ need to use tricks to get things done. the trick we use here is
\ using the wraparound of the 8 bit registers to get a bouncing ball.

\ state variable: point coordinate vector
variable px
variable py

\ system variable: point velocity vector
variable vx
variable vy

\ a new delay loop for pixels. we'd like at least 128 pixels
\ horizontal, so we have a bit more resolution than the 'usec' word.
  
: pix  for nop next ;
  
\ bounce will 'bounce wrap' a coordinate:
\ 0    ->   0 ...   #x00 -> #x00
\ 127  -> 127       #x7F -> #x7F
\ 128  -> 127 ...   #x80 -> #x7F
\ 255  ->   0       #xFF -> #x00
    
: bounce
    1st 7 high? if      \ check if the first item on stack has bit 7 set
	#xFF xor        \ if so, toggle all the bits
    then ;

\ read the coordinates with bouncing applied    
: get-px  px @ bounce ;
: get-py  py @ bounce ;

\ this will simply update the coordinate vector using the velocity vector
: animate
    vx @ px +!
    vy @ py +! ;

\ set the default values for the 2 vectors    
: animate-defaults
    20 px !
    20 py !
    2 vx !
    3 vy !
    ;

    
: screen-ball

    \ install the hook
    screen ->

    \ update the animation. we do this after an 'hsync' so we can
    \ steal time from a black line.
    
    hsync animate

    \ draw the lines
    get-py 10 + blanks
    10 for
	hsync
	get-px 1 + pix    \ 0 pix means 256 pix, so we just add 1 pix border here
	WHITE 1 pix BLACK
    next
    229 get-py - blanks ;
	

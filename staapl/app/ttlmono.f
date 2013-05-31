
\ Video signals and polarity.
macro
: video LATC 5 ;  
: hsync LATD 3 ;
: vsync LATD 2 ;
forth
: off       video low ;
: on        video high ;
: hsync-off hsync low ;
: vsync-off vsync high ;    
: hsync-on  hsync high ;
: vsync-on  vsync low ;    


macro    
: timer-if PIR1 TMR2IF ;
forth

: wait-timer
    begin timer-if high? until
    timer-if low ;
  
: init-timer
    137 PR2 !
    5 T2CON ! ;  \ 4 x PRESCALE + ON

    
: init-ttlmono

    TRISC 5 low \ RC5 output
    TRISD 3 low \ HSYNC
    TRISD 2 low \ VSYNC

    off  \ default (off) states
    hsync-off 
    vsync-off

    init-timer    
    ;

: pulse 5 for next ;    

\ 370 lines in total

: usec
    2 max          \ set the limit
    1 - for
        \ nop nop  \ add these 2 when running on 48 MHz
	nop nop nop nop
	nop nop nop
    next ;

: 1usec 
    nop nop nop nop
    nop nop ;
    

: line wait-timer hsync-on pulse hsync-off ;  
    
: test
    init-ttlmono
    begin
        \ Keep VSYNC low for one scanline duration.
        wait-timer vsync-on
        wait-timer vsync-off

        38 for
            10 for line
                20 usec
                100 for video toggle next
            next
        next
    again
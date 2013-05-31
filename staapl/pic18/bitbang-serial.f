
\ Timing

\ This is reasonably accurate for 9600 @ 8MHz and proportional.  For
\ lower/higher rates please see busyloop.f for details.


\ LSB first serial comm
macro
: bit>c | f b |  f b high? if stc      else clc     then ;
: c>bit | f b |         c? if f b high else f b low then ;    

\ Output
: bb-tx | 1bit port |       \ byte --
    port i low              \ start bit
    1bit i
    8 for
        rot>>c port i c>bit \ data bit
        1bit i
    next drop 
    port i high                \ stop bit
    1bit i ;

\ Input
: bb-rx | 1/2bit 1bit port | \ -- byte
\    begin
\        begin port i high? until \ wait for break/FE end
        begin port i low? until  \ wait for start bit
        1/2bit i                 \ sample halfway through bit
        1bit i
        0 8 for
            port i bit>c rot>>c \ data bit
            1bit i
        next
\    port i high? until  \ ignore FE
   ;

    
forth    
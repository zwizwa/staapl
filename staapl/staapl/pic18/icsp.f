
\ ICSP serial protocol with clock=PGC/RB6, data=PGD/RB7, setup on
\ clock 0->1 and latch on clock 1->0.

\ This is useful in conjunction with the PK2, which can be programmed
\ off-the-shelf to do all kinds of neat stuff.

\ Note that using PK2 the speed needs to be set a bit lower than max.
\ The highest speed has 1us period but 160ns pulses, which are too
\ narrow to see polling in software.  Configuring the PK2 speed to 3
\ (= 3us) seems fine for PK2 read.  It gives 1.5/1.5 us read pulses
\ and 1/2 us write pulses.

macro
: icsp-dir-in    TRISB 7 high ;
: icsp-dir-out   TRISB 7 low ;
: icsp-odata     LATB 7 ;
: icsp-idata     PORTB 7 ;
    
: icsp-clock     PORTB 6 ;
: icsp-rx>>      PORTB @ rot<<c drop rot>>c ; \ data -- data
: icsp-tx>> \ acc -- acc : shift bit into accumulator
    rot>>c c? if
        icsp-odata high
    else
        icsp-odata low
    then ;
: icsp-sync \ -- : sync on rising clock edge
    begin icsp-clock low?  until
    begin icsp-clock high? until
    ;
forth

\ Protocol is byte-oriented.  Note that these are primtives and do not
\ perform proper handshaking or bus management!
    
: icsp-rx \ -- byte
    0 \ accu
    8 for
        icsp-sync
        icsp-rx>>   
    next ;
    
: icsp-tx \ byte --
    8 for
        icsp-sync
        icsp-dir-out \ only assert line after clock arrived
        icsp-tx>>
    next
    \ leave output asserted to last bit
    drop ;



\ Synchronization is necessary as we can't assume the target is always
\ listening.  The hardware constraints are:
\
\    1. Use only PGD and PGC
\   
\    2. No external components (i.e. bus conflict resistors)
\
\    3. Work with bus management behaviour allowed by ummodified PK2
\       firmware (V2.32)


\ The target doesn't know when the PK2 releases any of the lines so it
\ can't signal without being asked (clocked).  Host doesn't know if
\ target is there.

\ The data line is pulled low when it's not asserted.  That gives us a
\ sync condition to start with: the target can write a single 1 bit in
\ response to a clock signal to indicate a ready condition, so the PK2
\ can start clocking a read or write.  We need another clocki cycle
\ for the target to safely release the line and continue with whatever
\ communication is next (rx or tx).  We write a 0 to allow for faster
\ settling as the pull-down fall time is slow.

: icsp-handshake \ --
    icsp-sync
      icsp-dir-out
      icsp-odata high  \ 1: assert
    icsp-sync
      icsp-odata low   \ 0: assert first, 
      icsp-dir-in ;    \    then hi-z

\ The postamble is necessary to hold the last transmitted bit for one
\ clock sample.  After sync we zero and release the line just as in
\ preamble.
    
: icsp-wait-ack \ --
    icsp-sync            \ if tx, this holds the last output bit
      icsp-dir-out
      icsp-odata low     \ 0: assert first,
      icsp-dir-in ;      \    then hi-z

    
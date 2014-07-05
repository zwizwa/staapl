\ port initialization synth board:
\ -- 5 analog inputs
\ -- 3 switch inputs with pullup
\ -- audio port output
\ -- pot net on/off output


\ notes

\ * since port initialization is a board feature, all inits, except
\ for the serial port, are concentrated here. it is OK to use some
\ chip macros for init though.

\ * AD inputs can be set individually. this is different than on the
\ 18f2550 for example, where you can only enable a contiguous range of
\ ports as analog inputs.

\ * to set an analog input, both digital input and output need to be
\ disabled. (see datasheet page 89, figure 10-2) the former is done by
\ clearing bits in the ADCON1 register (0 = analog in, 1 = digital
\ in), the latter is done by setting the TRIS bits, which disables the
\ output driver.


macro

: init-ports-digital 
    init-ports-digital-pk2icsp ;    

: init-ports-digital-pk2icsp
    \ Just stay away from PORTB for ICSP protocol.  Digi in won't work.
    ;
    
: init-ports-digital-pk2serial
    \ #xE1 TRISB or!    \ analog 4 is RB0 + RB5-RB7 are digital in
    #x61 TRISB or!    \ analog 4 is RB0 + RB5-RB7 are digital in (RB7 = icd tx)
    INTCON2 RBPU low ; \ enable weak pullups for port B (switches)

: init-ports-analog
    \ note that PGM is still configured as LVP. the pin is open so it
    \ needs pullups.

    #x1F TRISA or!    \ disable digital output for A0-A4
    TRISA 7 low       \ RA7 is output: potnet on/off switch

    #x60 ADCON1 !     \ set A0-A4 as analog input (5,6 digital for serial port)
    #x7E ADCON2 !     \ AD: left justified, 20 TAD, fosc/2
;


: S1?   PORTB 7 low? ; 
: S2?   PORTB 6 low? ; 
: S3?   PORTB 5 low? ;    
    
forth

\ these are useful as interactive commands, so not defined as macros.  
: potnet-on  LATA 7 high ;
: potnet-off LATA 7 low ;   


: param

    \ first 3 pots set period
    
    0 ad@ p0
    1 ad@ p1 
    2 ad@ p2

    \ pot bits are XOR FF because they are upside down.
    
    \ pot nb 3 is [ noise:1 synth:2 ignored:5 ] 
    3 ad@ #xFF xor dup

    \ set noise bit synth:7  (FIXME: check this / was moved)
    rot>>4
    #b00001000 synth-bits!

    \ set mixer algo bits synth:1-0
    rot<<3
    #b00000011 synth-bits!
    
    \ pot nb 4 sets sync bits [ sync: 3 ignored: 7 ]

    4 ad@ #xFF xor rot>>
    #b01110000 synth-bits!

    ;


    
\ board init, leaves serial port config alone.    
: init-board
    init-ports-digital
    init-ports-analog
    potnet-on ;
    
    

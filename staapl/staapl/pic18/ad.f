\ pic18 analog->digital conversion

\ this is currently hardcoded for the CATkit board. i had some trouble
\ getting it to work, so i'm just following instructions.

\ 18LF1220: DS39605C p. 161
\ 18LF2620: DS39626C p. 223

macro
: ad-ready? ADCON0 GO_DONE low? ;
: init-ad   #b00111010 ADCON2 ! ;   \ left justified, 20 TAD, FOSC/32
forth

\ turn on the AD covertor
\ ref = VDD/VSS
\ ( channel -- )
: ad-channel! 7 and << << 1 or ADCON0 ! ;
: ad-start    ADCON0 GO_DONE high ;    

\ wait for a conversion to finish, then collect the high bits
: ad@-sync    begin ad-ready? until \ fallthrough
: ad@-nosync  ADRESH @ ;
    
: ad@
    ad-channel!
    ad-start
    ad@-sync ;

\ registers of interest:
\ ADCON1 for analog/digital select
\ TRISA/TRISB for input/output select
  
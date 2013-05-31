\ Macros for handling and initializing interrupts on PIC18
\ hardware. This is tested on 18F1320 and 18F2620

macro  

\ DISPATCH
  
: ack/dispatch | iflag isr |
    iflag compile high? if
	iflag compile low
	isr compile
	1 retfie
    then ;

\ GLOBAL

\ The following macros are orthogonal. The final command to switch on
\ interrupts is always sti, which is defined elsewhere as "INTCON GIE
\ high". The global contract: macros for specific devices ONLY access
\ their own flags, not the global interrupt enable.
    
: interrupt-init    
    RCON IPEN high       \ enable priority levels
    INTCON PEIE high     \ enable peripheral interrupts
;    
\ TMR2

: tmr2-ie PIE1 TMR2IE ;
: tmr2-if PIR1 TMR2IF ;

\ FIXME: doesn't set PR2    
: tmr2-init

    IPR1 TMR2IP high    \ high priority interrupt
    #x04 T2CON !        \ no scaling, timer on
    \ #x3F PR2 !          \ TMR2 period (31.25 kHz)
    \ #xFF PR2 !          \ debug: slower
    0 TMR2 ! ;
    
: tmr2-on  tmr2-if low tmr2-ie high ;
: tmr2-off tmr2-ie low ;
    
\ INT0    

: int0-ie INTCON INT0IE ;
: int0-if INTCON INT0IF ;

: int0-init \ FIXME: piggybacks on tmr2-init
    INTCON2 INTEDG0 high   \ rising edge
    ;

: int0-on  int0-if low int0-ie high ;
: int0-off int0-ie low ;

    
forth

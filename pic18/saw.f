\ Saw tooth oscillator based on comparator 1

load spi.f


macro
: /DISCHARGE LATD 0 ;
: comparator CMCON C1OUT ;
forth

: init-comparator
    TRISD 0 low  \ configure /DISCHARGE output
    TRISA 0 high \ AN0 -
    TRISA 3 high \ AN3 +
    TRISA 4 low  \ RA4 is C1OUT
    TRISA 5 low  \ RA5 is C2OUT
    #x03 CMCON ! \ 2 independent comparators
    \ todo configure vref
    \ configure interrupts: device, priority, ...

    PIE2 CMIE low    \ disable CM interrupts
    IPR2 CMIP high   \ high pri
    PIR2 CMIF low    \ clear interrupt flag
    PIE2 CMIE high   \ enable CM interrupts

    INTCON PEIE high \ enable peripheral interrupts
    INTCON GIE high  \ enable global interrupts
    
    ;


: hi-isr
    8 org-begin
\    0 ,,
\    0 ,,
\    0 ,,
\    0 ,,
    /DISCHARGE low  \ drive PNP
    PIR2 CMIF  low  \ ack interrupt
    /DISCHARGE high \ release PNP
    1 retfie        \ fast return
    org-end



: init-saw
    init-comparator
    mcp4922-init-spi
    ;

: A mcp4922-tx-A ;
: B mcp4922-tx-B ;        
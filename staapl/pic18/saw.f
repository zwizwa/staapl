

macro
: discharge LATA 0 ;  
forth

: hi-isr
    discharge high
    PIR2 CMIF low   \ ack interrupt
    discharge low
    1 retfie        \ fast return

' hi-isr init-isr-hi

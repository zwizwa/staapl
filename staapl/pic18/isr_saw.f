\ fixme: this installs an ISR so file is prefixed with ISR.
\ apparently 'staapl boot' causes problems (for init-isr-hi)



macro
: discharge LATA 0 ;  
forth

: hi-isr
    8 org-begin
    discharge high
    PIR2 CMIF low   \ ack interrupt
    discharge low
    1 retfie        \ fast return
    org-end


\ The "pro" version: Interrupt driven

macro    
: install-timer-isr
    8 org-push
      timer-if low \ ack
      pop 1 retfie \ fall out of loop + return
    org-pop ;
forth

install-timer-isr

: init-timer 160 PR2 ! 5 T2CON ! ;


\ timer interrupt flag
macro    
: timer-if PIR1 TMR2IF ;
forth


\ Timer Interrupts
: init-interrupts
    IPR1 TMR2IP high  \ TMR2 high priority
    timer-if low      \ clear flag
    PIE1 TMR1IE high  \ enable TMR2 interrupts
    INTCON PEIE high  \ enable peripherial interrupts
    ;
    
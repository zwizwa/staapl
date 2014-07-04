load bitbang-serial.f
load busyloop.f

\ A non-invasive serial console over the PGC/PGD pins.  Apart from the
\ 2 pins this requires no resources.  Downside is that for reliable
\ command transfer, the RX/TX can't be interrupted.  If that's a
\ problem, use the hardware serial port.

macro
: icd.rx-port PORTB 6 ;  \ PGC
: icd.tx-port LATB 7 ;   \ PGD
: init-icd-serial
    TRISB 6 high
    TRISB 7 low ;
: icd.rx-ready?    icd.rx-port low? ; \ For external polling
forth

: icd.1bit   icd.1/2bit 
: icd.1/2bit baud half-bit ;
: icd.rx>    ' icd.1/2bit ' icd.1bit ' icd.rx-port bb-rx ;
: icd.>tx                 ' icd.1bit ' icd.tx-port bb-tx ;    

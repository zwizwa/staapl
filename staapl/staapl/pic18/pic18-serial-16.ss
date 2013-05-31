#lang planet zwizwa/staapl/pic18 \ -*- forth -*-
provide-all

staapl pic18/pic18-serial

macro

\ Generic

: !! | value high low |
    value low !
    value #x100 / high ! ;
  
: async.init-brg-16 | baud fosc |

    \ Select 16 bit mode with divisor = 4.  Higher baud rates are
    \ possible when TXTA BRGH is also set, which uses a divisor = 4.
    \ See 39632B page 237 (18F2550)
    fosc baud 4 fosc/baud/div>count SPBRGH SPBRG !!

    #x24 TXSTA !               \ enable transmission and high baud rate
    #x90 RCSTA !               \ enable serial port and reception    
    BAUDCON BRG16 high         \ enable 16 bit operation
;

forth
  
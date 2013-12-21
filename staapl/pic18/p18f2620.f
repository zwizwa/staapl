\ chip constants
load p18f2620-const.f
staapl pic18/pic18-serial
staapl pic18/pic18-serial-16

macro

\ Chip config macros for core code

: init-chip
    -1 LATC !        \ prevent TX pin glitch
    #x70 OSCCON ! ;  \ internal oscillator at 8MHz


\ Detect the presence of a serial cable by detecting an IDLE state
\ (high) on RX (RC7). Tie the pin low using 100k to default to BREAK
\ with no cable connected, or jumper it.

\ This macro is only valid AFTER initializing the serial port, which
\ sets the inputs to digital.
    
: debug-serial?
    PORTC 7 high? ;

: init-serial | baud fosc |
    \ see DS39626C-page 211
    #b11000000 TRISC or!     \ pin config
    baud fosc async.init-brg-16     
    ;

: init-board ;

\ A/D converter
\ DS39626C p. 224

\ Ports are configured as analog using a range, not by individual
\ ports. This a bit awkward, so do this manually in the app.

: boot-size #x800 ;   \ size of code-protect region
    
forth

  
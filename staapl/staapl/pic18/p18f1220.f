\ chip constants
load p18f1220-const.f
staapl pic18/pic18-serial
macro
  
: serial-div 16 ;   \ High baud rate with 8 bit counter.

\ chip config macros for core code

\ FIXME: should i set chip pins to output? according to johannes
\ inputs shouldn't hurt.

: init-chip
    -1 LATB !        \ prevent TX pin glitch
\   2-output         \ 2 ports, all output
    #x70 OSCCON ! ;  \ internal oscillator at 8MHz



\ Detect the presence of a serial cable by detecting an IDLE state
\ (high) on RX (RB4). Tie the pin low using 100k to default to BREAK
\ with no cable connected, or jumper it.

\ On a Power-on Reset, RB4:RB0 are configured as analog inputs by
\ default and read as 0; RB7:RB5 are configured as digital
\ inputs. This means we need to set ADCON1 to check the debug state.

\ This macro is then only valid AFTER initializing the serial port,
\ which sets the inputs to digital.
    
: debug-serial?
    PORTB 4 high? ;
    

    
: init-serial | baud fosc |
    ADCON1 5 high ADCON1 6 high  \ pins
    TRISB 4 high TRISB 1 high
    \ init-serial-baud-8           \ baud rate
    baud fosc async.init-brg-8
    ;

: boot-size #x200 ;   \ size of code-protect region

forth

  
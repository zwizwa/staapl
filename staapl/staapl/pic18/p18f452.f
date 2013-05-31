\ chip constants
load p18f452-const.f
staapl pic18/pic18-serial

macro
  
\ chip config macros for core code
: init-chip ;  \ nothing needed    




\ Detect the presence of a serial cable by detecting an IDLE state
\ (high) on RX (RB4). Tie the pin low using 100k to default to BREAK
\ with no cable connected, or jumper it.

\ On a Power-on Reset, RB4:RB0 are configured as analog inputs by
\ default and read as 0; RB7:RB5 are configured as digital
\ inputs. This means we need to set ADCON1 to check the debug state.

\ This macro is then only valid AFTER initializing the serial port,
\ which sets the inputs to digital.
    
: debug-serial?
    PORTC 7 high? ; \ FIXME: check this
    
: init-serial | baud fosc |
     TRISC 7 high TRISC 6 low     \ pins
     baud fosc async.init-brg-8 ;
forth

  
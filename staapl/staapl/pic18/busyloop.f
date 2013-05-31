
\ This is optimized for fosc from 8 -> 48.  The inner loop compensates
\ for the oscillator period.  Using a convenient period of 50us, this
\ gives 33 iterations at 8Mhz and 200 iterations at 48Mhz.  The macro
\ is exposed, but it is most accurate for +- 50 us.

macro
: usec
    fosc 4000000 / *  \ instructions per us
    3 /               \ instructions per loop
    for next ;

: 50usec 50 usec ;
: 10msec 200 for 50usec next ;
: 1sec   100 for 10msec next ;


\ Periods for standard PC baud rates.  I.e. for 9600 baud we need
\ half-bit times of 52.08 usec, which is about the unit used above.

: half-bit | rate |
    1 2 rate * /      \ half bit period (s)
    1000000 * usec ;

forth




\ the hardware is very simple:


\ 2 bit TV output DAC. simple resistor ladder. with Vcc = 5V, this creates
\ a voltage divider with the 75 Ohm input impedance giving the mV levels
\ 0 [sync], 333 [black], 666 [grey] and 1000 [white]
\
\             1000
\    V0 >---/\/\/\/\/\---\
\                        |
\              500       |
\    V1 >---/\/\/\/\/\---|
\                        |
\                        \----> RCA signal pin 
\
\ instead of the 500 ohm resistor, you can use 2 x 1K in series.

\ the resistor network is connected to PORT A, bit 0 and 1. we use
\ some constants to identify the ports so they are easy to change
\ later.

macro
\ : V0 LATA 0 ;  \ least significant bit
\ : V1 LATA 1 ;  \ most significant bit
: V1 LATC 5 ;  \ MSB = serial data out
: V0 LATA 5 ;  \ LSB    
  
forth
  
\ this enables those pins as outputs
: init-ports
    \ TRISA 0 low
    \ TRISA 1 low
    TRISC 5 low
    TRISA 5 low
    ;

\ video level switching: these are implemented as macros so they have
\ a predictable execution time: 2 instructions long.
    
macro
: SYNC   V0 low  V1 low ;
: WHITE  V0 high V1 high ;
: GREY   V0 low  V1 high ;    
: BLACK  V0 high V1 low ;
forth

\ microsecond delay loop, used for sync puls shaping. this assumes
\ we're running at 48Mhz clock, which means 12MHz instruction cycle,
\ so a single microsecond has 12 instructions in it. have a look at
\ the asm output: loop overhead is 3 instructions.

\ NOTE: - minimum is 2 usec !!
\       - there's another usec in busyloop.f

  
: _usec
    2 max        \ set the limit
    1 - for
	\ nop nop  \ remove these 2 when running on 40 MHz
	nop nop nop nop
	nop nop nop
    next ;

: 1_usec
    nop nop nop nop
    nop nop
    ;

\ we use an internal timer to obtain the line frequency, which is
\ 15.625 kHz, or 64usec.

\ so, 64usec @ 12MIPS = 192 with 4x scaling
\              10MIPS = 160    
: init-timer 160 PR2 ! 5 T2CON ! ;


\ timer interrupt flag
macro    
: timer-if PIR1 TMR2IF ;
forth


    
\ ISR

\ wait for line frequency timer
\ NOTE: to get zero jitter (here it's 0-1-2) use interrupt driven sync.
  
: line
    begin timer-if high? until  \ wait for interrupt flag being set
    timer-if low                \ set low, and continue
    ;

\ HSYNC : run this at the start of a line, before starting to draw
: hsync
    line          \ wait for next timer tick
    SYNC  4 _usec  \ output sync level for 4 usec
    BLACK 4 _usec  \ output black level for 8 usec
    \ HACK: for 40Mhz!!
;

\ draw a line with a white bar on it    
: barline
    hsync
    20 _usec WHITE
    10 _usec BLACK ;


\ draws a number of blank lines
: blanks \ n --
    for hsync next ;

: vsync0
    for
	line
	SYNC 28 _usec
	BLACK 4 _usec
	SYNC 28 _usec
	BLACK
    next ;

: equalize
    for
	line
	SYNC   4 _usec
	BLACK 28 _usec
	SYNC   4 _usec
	BLACK
    next ;

\ VSYNC: multiple lines, with 2 different SYNC/BLACK patterns
: vsync
    3 equalize
    3 vsync0
    3 equalize ;

\ PAL = 625 lines interlaced. we're doing half of that: 313, of which
\ 63 are sync and blank, which leaves 250 lines to fill up by the
\ screen drawing routine.
    
: borders
    \ usable screen here
    18 blanks   \ bottom border
    vsync       \ 9 lines vertical sync sequence
    36 blanks   \ top border
    ;  


\ once the sync generator is working, we can put that functionality in
\ a loop and work with a callback (hook)
    
load hook.f
macro : screen 0 ; forth

\ just the display loop
: display
    init-ports
    init-timer
    begin
	screen run-hook \ run the hook
	borders
    rx-ready? until ;

: screen-test
    screen -> \ this sets the hook to the following code
    
    250 for
	barline
    next ;


    
: _sync SYNC ;
: _black BLACK ;
    
    
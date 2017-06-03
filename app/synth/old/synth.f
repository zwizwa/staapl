\ -*- forth -*-

\ sheepsint is modeled after synth14 running on the pic12f628. a
\ simple 1 bit synth with 3 oscillators. this one is supposed to be a
\ bit more 'high quality'.

\ some (possible) additions compared to PIC12

\ * asynchronous: 3 hardware 16bit timers using interrupts, gives
\   higher 'sampling rate' for oscillators
\ * multiplier based exponential/sinusoidal modulation
\ * self-programming
\ * high level 'config forth'


\ 2 oscillator mixer configurations

\ * XMOD = XOR, with possible assymetric sync
\ * RESO = 0 AND [1 XOR 2], where 2 acts as an envelope (one-shot)


\ using the 4 timers:

\ TMR0 16bit OSC0 main osc
\ TMR2 16bit OSC1 noise / formant
\ TMR3 16bit OSC2 reso
\ TMR1 8bit  (fixed) control frequency


\ lowlevel dependencies:

staapl pic18/execute
staapl pic18/vector
staapl pic18/route
staapl pic18/prom  \ FIXME: sti
staapl pic18/_table
staapl pic18/task


\ application

load core.f
load control.f
load soundgen.f

load patch.f
load example.f

load sounds.f

\ load demo.f

\ TODO: control using the 16bit VM. see pic18/direct.f
\ load direct.f

\ macro
\ : synth-loop  \ word --
\     >m on
\     begin
\ 	cswap m>word
\ 	rx-ready?    \ stop looping when serial byte received
\     until
\     off ;  
\ forth  


\ Loop until byte received.  
\ : mainloop  ' param synth-loop ;




\ start application. this runs after the interpreter init is complete.

  
: main
    init-board     \ synth board specific inits
    engine-on      \ turn on syth engine (interrupts)

: mainloop
    param mainloop ;


macro

: install  ' isr init-isr-hi ;
    
forth


\ install by default
install

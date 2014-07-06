\ sheepsint audio core module

macro
: OSC0-enable INTCON TMR0IE ;
: OSC1-enable PIE1   TMR1IE ;
: OSC2-enable PIE2   TMR3IE ;
: OSCF-enable PIE1   TMR2IE ;

: OSC0-flag INTCON TMR0IF ;
: OSC1-flag PIR1   TMR1IF ;
: OSC2-flag PIR2   TMR3IF ;
: OSCF-flag PIR1   TMR2IF ;
forth    

\ fixed tick counter for binary time base : control locks here
variable tick0
variable tick1
variable tick2
variable tick3

\ oscillator periods
variable posc0l variable posc0h
variable posc1l variable posc1h
variable posc2l variable posc2h

\ sample period
variable psample

\ synth flags
\ 0-2 mix:
\   0 silence
\   1 xmod
\   2 reso
\   3 osc1  
\ 3 square/noise switch for osc1  
\ 4 sync 0->1
\ 5 sync 0->2  
\ 6 sync 1->2  
\ 7 unused
  
variable synth

\ 01 mixer : needs to be the same as 'mix'
macro  
: mix:silence 0 ;
: mix:xmod    1 ;
: mix:reso    2 ;
: mix:osc1    3 ;
forth    

macro  
: osc1:noise 3 ; \ osc1 noise/square switch
: sync:0->1  4 ;
: sync:0->2  5 ; \ will put 2 in one-shot mode
: sync:1->2  6 ;
forth  

\ the ordering here is just so we can piggyback on full adder compute
\ use easy osc0 xor [osc1 and osc2]
variable osc-bus
macro
: bus:0 osc-bus 2 ;
: bus:1 osc-bus 1 ;
: bus:2 osc-bus 0 ;
forth
  
\ output buffer -- FIXME: really needed?
variable output-buffer
macro  
: output output-buffer 0 ;
forth


macro

: mixer       3 and ;  \ synth -- mixer
  
: output<c    output-buffer rot<<c! ;

: AUDIO+      LATB 3 ;
: AUDIO-      LATB 2 ;

: init-out
    TRISB 2 low
    TRISB 3 low ;
    
\ : speaker-on  #x0C LATB or! ;
\ : speaker-off #xF3 LATB and! ;    

: speaker-on    AUDIO- low  AUDIO+ high ;
: speaker-inv   AUDIO- high AUDIO+ low ;

: speaker-off   AUDIO- low  AUDIO+ low ;    
    
forth

\ sample player
variable sample-bits
variable sample-bucket
variable sample-lo
variable sample-hi

\ control
variable pole

  
staapl pic18/lfsr \ rng
  

\ SYNTH


\ oscillator updates:
\   restart is ran once / cycle to ADD to the timer, so the period is
\   stable no matter when in the cycle this routine is called. updates
\   assume no carry : handled within 247 counts from interrupt

: restart0
    TMR0L @      \ read low timer value. this also latches the high byte.
    9 +          \ 7 = space between updates, 2 = TMR0 delay
    posc0l @ +   \ advance timer lo, write carry bit
    posc0h @     \ read timer high increment on stack
    TMR0H ++!    \ update timer high latch
    TMR0L ! ;    \ write out timer lo, which also writes the latched high byte.
    
\ same as TMR0, only TMR1/TMR3 do not have a 2 cycle delay after write.
    
: restart1 TMR1L @ 7 + posc1l @ + posc1h @ TMR1H ++! TMR1L ! ;
: restart2 TMR3L @ 7 + posc2l @ + posc2h @ TMR3H ++! TMR3L ! ;

\ oscillator updates: reset really resets a timer. also clear
\ interrupt flag to kill pending interrupts.

: reset0 posc0h @ TMR0H ! posc0l @ TMR0L ! ;
: reset1 posc1h @ TMR1H ! posc1l @ TMR1L ! ;
: reset2 posc2h @ TMR3H ! posc2l @ TMR3L ! ;


\ mix bus to audio output
: bus>audio
    mix
: click
    output high? 
    if speaker-on ; then
    speaker-inv ;
\    speaker-off ;
    

\ output mixers
 

\ reso filter emulation: this requires some explanation.

\ the output = [osc0] toggled by [osc1 and osc2]
\ the and operation combines the reso response [osc1] with an envelope [osc2]
\ this consists of the transient response, which is added to [osc0]

\ the 'and' operation is symmetric, so which of both inputs is the
\ envelope depends on the triggering mechanism: [osc2] is one shot if
\ it is synced to [osc0]
    
\ now, in order to compute this, i use the fact that the bits are ordered as
\ [osc0] : [osc1] : [osc2]

\ adding 1 to this will use the binary addition logic to carry over
\ the bit in case both [osc1] and [osc2] are one, so the result of
\ [osc1 and osc2] is xored with osc0.

    
: mix-reso
    osc-bus @ 1+  \ perform logic op
    rot>> rot>>   \ get result bit in LSB
    output-buffer ! ;
    

\ and a hack for xmod
\ [osc2] is copied by shifting it into output reg
\ [osc1] is shifted to carry flag, then carry and
\ [osc0] are added to output-buffer
: mix-xmod
    osc-bus @ rot>>c output<c
              rot>>c output-buffer ++! ;

: mix-silence
    0 output-buffer ! ;

: mix-osc1
    osc-bus @ rot>>
    output-buffer ! ;
    
\ compute output from oscillator bus depending on synth algo.
: mix
    synth @ mixer route
	mix-silence .  
	mix-xmod .     \ OSC0 xor OSC1 xor OSC2
	mix-reso .     \ OSC0 xor (OSC1 and OSC2)
	mix-osc1 ;     \ OSC1

	
    




    


 macro
: do-isr | reg bit word |
    reg bit high? if
        reg bit low
        word i 1 retfie
    then ;
    
\ : m2>     m-swap m> ;
\ : m2>word m-swap m>word ;   
  
\ : do-isr    
\     >m     \ word >m
\     >m >m  \ flag >m
\     m-2dup
    
\     m> m> high? if  \ note that 'if' uses compile stack
\ 	m2> m2> low
\ 	m2>word 1 retfie
\     then ;
forth    
  
: isr
    OSCF-flag ' fixed do-isr
    
    OSC0-flag ' osc0  do-isr
    OSC1-flag ' osc1  do-isr
    OSC2-flag ' osc2  do-isr
    
    reset  \ fallthrough

    
\ same design as sheepsint: square wave oscillators with sync here,
\ oscillator events come from system timer events restart before
\ 'click' since restart is a constant delay. sync reset after all the
\ rest.


\ main sync source    
: osc0
    restart0
    bus:0 toggle 

    synth sync:0->1 high? if reset1 bus:1 high then
    synth sync:0->2 high? if reset2 bus:2 high then

    bus>audio ;

\ formant frequency or noise    
: osc1
    restart1
    synth osc1:noise high? if
	rand>c c? if bus:1 high else bus:1 low then
    else
	bus:1 toggle
    then

    synth sync:1->2 high? if
	reset2 bus:2 high
    then

    bus>audio ;

\ resonance    
: osc2
    restart2
    synth sync:0->2 high? if
	bus:2 low     \ if synced to 0, do one shot
    else
	bus:2 toggle  \ else just osc
    then  

    bus>audio ;
    

\ the fixed rate interrupt by default runs at FOSC/256, which is 7812
\ Hz. this increments a 32bit timer that can be used for control
\ sync. use PER2 to set the frequency to FOSC/(PER2+1)
    
: fixed
    dup WREG subwf  \ 0 stc
    dup tick0 ++!
    dup tick1 ++!
    dup tick2 ++!
        tick3 ++! ;




\ enable everything except the global interrupt flags which are used
\ to switch the synth on/off
    
: init-timers

    INTCON2 TMR0IP high \ TMR0: high priority    
    IPR1 TMR1IP high
    IPR1 TMR2IP high
    IPR2 TMR3IP high
    \ init-tcons-scale1
    init-tcons-scale4
    \ init-tcons-scale8/16
    RCON IPEN high      \ enable priority levels
    #x3F PR2 !          \ TMR2 period (31.25 kHz)
    ;

macro    
: init-tcons-scale1
    #x88 T0CON !         \ TMR0: on, internal, 16bit, no prescale
    #x81 T1CON !         \ TMR1: 16bit reads, no scaling
    #x04 T2CON !         \ TMR2: no scaling
    #x81 T3CON !         \ TMR3: 16bit reads, no scaling
    ;
\ same but add 1:4 prescale (TMR2 doesn't support 1:8, the others do)    
: init-tcons-scale4      
    #x80 #x02 or T0CON ! \ 1:4
    #x81 #x20 or T1CON ! \ 1:4
    #x04 #x01 or T2CON ! \ 1:4 
    #x81 #x20 or T3CON ! \ 1:4
    ;
: init-tcons-scale8/16      
    #x88 #x03 or T0CON ! \ 1:16
    #x81 #x30 or T1CON ! \ 1:8
    #x04 #x02 or T2CON ! \ 1:16 
    #x81 #x30 or T3CON ! \ 1:8
    ;
forth
    
: ion    
    \ INTCON GIEH low
    INTCON TMR0IF low
    PIR1   TMR1IF low
    PIR1   TMR2IF low
    PIR2   TMR3IF low
    INTCON TMR0IE high
    PIE1   TMR1IE high
    PIE1   TMR2IE high
    PIE2   TMR3IE high
    INTCON GIEH   high
    \ sti
    ;
    
: ioff
    INTCON TMR0IE low
    PIE1   TMR1IE low
    PIE1   TMR2IE low
    PIE2   TMR3IE low
    ;


\ individual interrupt enables

\ : ioff-0  OSC0-enable low ;
\ : ioff-1  OSC1-enable low ;
\ : ioff-2  OSC2-enable low ;
\ : ioff-f  OSCF-enable low ;   

\ : ion-0   OSC0-enable high ;
\ : ion-1   OSC1-enable high ;
\ : ion-2   OSC2-enable high ;
\ : ion-f   OSCF-enable high ;    


    
    
\ FIXME: programming enables interrupts
: _engine-on
: engine-on
    init-timers         \ setup 18f hardware timers
    init-rand           \ setup lfsr RNG
    init-out            \ setup audio output
    ion ;

: _engine-off
: engine-off
    ioff
    speaker-off ;    \ both leads are in 0 position.

    


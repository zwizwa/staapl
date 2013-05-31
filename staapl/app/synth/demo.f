\ CATkit demonstration. this uses all knobs and switches.

load 31note.f
load mul16-global.f

\ buttons:
\ * noise on/off
\ * xmod
\ * reso
\ * reset = silence

\ take button state from ram, uninitialized, so it survives reset.

\ xmod control uses 2 x 2Hx - 20kHz log
\ reso needs robustness for reso freq < main osc freq

\ 3 x frequency nobs
\ 1 x OSC0 noise modulation

\ 2 knobs left.. maybe some modulation? osc 2 frequency + modulation
\ index. (formant / noise frequency)


\ base frequency for the modulation
 
\ variable osc2-lo
\ variable osc2-hi




    
\ FILTER Q

\ The most interesting control is constant Q, which means ring time
\ proportional to ring period. This requires a bit of shifting (Q
\ range) and a 2 x 1 signed multiplication.

macro
: invert  #xFF xor ;
forth

\ This produces a negative value that can be added to a period. 0 maps
\ to 0 0. Negative numbers are used so for small values of modulation
\ index (mi) the effect doesn't overflow from low -> high period. For
\ high modulation index this doesn't matter, since it's noisy anyway.

\ Hmm.. that's not really true: not good to overflow..  
  
macro
: PROD PRODL ;  
: _>>! | reg |
    clc
    reg 1 + rot>>c!
    reg rot>>c! ;
: _invert@ | reg |
    reg @ invert 1 +
    reg 1 + @ invert 0 ++ ;
    
forth

: PROD>>!   
    PROD _>>! ;
    
: mi->randperiod \ mi -- lo hi
    randbyte umul>PROD
    PROD>>!
    PROD>>!
    PROD>>!
    PROD _invert@
    ;

: noisemod \ pl ph mi -- pl+ ph+
    >x _dup x>
    
    mi->randperiod _+

    nc? if
        _drop ;  \ saturate instead of wrap
    then
        _nip ;

\ : ^^noisemod \ pl ph mi -- pl+ ph+
\     _dup
\     randbyte u**  \ modindex -> double mod depth
\     _fumul        \ modulated period
\     _- ;
    

: mainloop    
    S1? if
        mix:xmod
        sync:0->2 bit synth !
    then
    
    S2? if
        mix:reso
        sync:0->1 bit
        sync:0->2 bit synth !
    then
    
    S3? if
        synth osc1:noise high
    then
    
    0 knob->31period

    3 ad@ noisemod _p0

    1 knob->31period _p1
    2 knob->31period _p2
    
    mainloop ;

    
\ replaces default main    
: main
    init-board
    engine-on
    \ xmod3
    mainloop ;
    
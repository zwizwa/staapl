\ midi voice control

\ Voices are hardcoded.
: control-update
    \ don't update anything when silent
    synth @ 0 = if ; then
: pitch-update
    voice @ 3 min route
        v-square . v-pwm . v-reso . v-noise ;
        

: v-p0 period 2@ _p0 ;
        
: v-square v-p0 square ;
: v-pwm    v-p0 modwheel @ 0 _<<3 _pwm ;
: v-reso
    v-p0
    mix:reso
    sync:0->1 bit
    sync:0->2 bit synth !
    cutoff @ midi-note _p1 \ cutoff tuned to midi note
    _p0@ _>> _p2           \ fixme: ring fixed at 1/2 of period
    ;

: v-noise
    mix:osc1
    osc1:noise bit synth !
    -1 p0 -1 p2 \ disabled
    period 2@ _p1 ;
    
: _<<2 _<< _<< ;
: _<<3 _<<2 _<< ;    
: _<<4 _<<2 _<<2 ;
    
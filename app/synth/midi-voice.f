\ midi voice control

\ Voices are hardcoded.
: control-update
    \ don't update anything when silent
    synth @ 0 = if ; then
: pitch-update
    voice @ 1 min route
        v-square . v-pwm ;
        

: v-p0 period 2@ _p0 ;
        
: v-square v-p0 square ;
: v-pwm    v-p0 modwheel @ 0 _<<3 _pwm ;

: _<<2 _<< _<< ;
: _<<3 _<<2 _<< ;    
: _<<4 _<<2 _<<2 ;
    
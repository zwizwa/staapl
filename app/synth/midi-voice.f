\ midi voice control

\ Voices are hardcoded.
: voice-update
    voice @ 1 min route
        v-square . v-pwm ;

: v-p0 period 2@ _p0 ;
        
: v-square v-p0 square ;
: v-pwm    v-p0 1 1 _pwm ;

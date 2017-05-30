\ midi voice control

\ Voices are hardcoded.
: v-update
    voice @ 1 min route
        square . v-pwm ;

: v-pwm 1 1 _pwm ;
    
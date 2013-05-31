\ old patch stuff
: wait0 for 0 for next next ;    
: wait1 for 0 wait0 next ;



variable current-octave
: o! note current-octave @ for _>> next _p0 ;

    

: z0 \ x --
    for randbyte 7 and current-octave ! randbyte o! 50 wait0 next ;

: z1 \ x --
    for
	randbyte 3 and current-octave !
	randbyte o!
	randbyte pwm
	50 wait0
    next ;
 
: z2 \ x --
    for
	randbyte #x3f and 1+ z1
	randbyte #x3f and 1+ wait0
    next ;

    
: wioew     100 pole ! square 3 p0 10 P++ ;


: rrxmod  randbyte for rxmod 50 wait0 next ;

: nzwioew  for 10 z2 wioew next ;    

\ FIXME: this clashes with carry flag.    
\ : C 0 o! ;
\ : D 2 o! ;

\ demo words


: iiuu   1 p0 square 100 pole ! 100 P+ ;
: woe    100 p0 square 50 pole ! 100 P+ ;

: iiuwoe for iiuu woe next ;   

    
    
    
\ transient controller

\ code that runs at control rate is not nearly as time-critical as code
\ that runs at synth rate, though most things you'd want to at this rate
\ are still fairly simple timbre modulations.

\ one of those is transient sounds, like small noise bursts or
\ beeps. since we can't really mix sounds, we're going to mute the more
\ stable background tones whenever a transient occurs. when the
\ transient is done, all the synth params that were changed will be
\ restored from data pushed to the transient stack after the transient
\ counter is done.


\ testing

: pole+ _p0@ pole @ 0.0pole+ _p0 ;
: pole- _p0@ pole @ 0.0pole- _p0 ;

: P+  for pole+ 0 for next next ;
: P-  for pole- 0 for next next ;

: P++ for 0 P+ next ;
: P-- for 0 P- next ;


: Q+  for pole+ 10 for next next ;
: Q++ for 0 Q+ next ;




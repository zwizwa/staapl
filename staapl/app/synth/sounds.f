\ Some sound examples. A way to test them on the console would be:
\
\   wobble 10 start notes
\
\ The 'start' is just to prevent compaints about timeouts, and only
\ works at the console.



\ SYNC time scales

\ The fixed timer interval is at 31.25 kHz. The 'sync' bits start at
\ half of this: 15.625 kHz. From there on, each bit to the left is
\ half.

: fast-sync   6 sync ;   \ fast modulation of bassdrum pitch
: mod-sync    9 sync ;   \ used for slow modulation
: note-sync  11 sync ;   \ 1/16 th notes

\ SOUND
    
\ the pattern of code in a sound generation is always an infinite loop
\ with configuration changes interleaved with time synchronization
\ points.
    
: wobble
    sound -> \ set the 'sound' vector
    begin \ infinite sound loop 
	\ config change  \ time sync
	square 50 p0     mod-sync
	noise            mod-sync
    again


\ a bassdrum is a fast glide ..    
: bassdrum
    sound ->
    10 p0 square \ .. from high pitch to low
    240 for
	\ .. incrementing period once per fast sync
	p0@ 1 + p0 fast-sync 
    next
    \ 'tail' oes nothing but looping over yield, use this if the sound
    \ stays constant: at the end of the bassdrum we just let it sound
    \ like a bass. duration is determined by 'bang' in the control
    \ task.
    tail 

\ a snaredrum is just a noise burst..    
: snare
    sound ->
    noise
    7 for mod-sync next 
    silence tail \ .. with silence at the end


\ some pwm sound
: detune-pwm
    sound ->
    100 p0 50 pwm
    tail


\ The idea behind the formant synt is:
\  _   _   _   _   _   ___   _   _
\ | |_| |_| |_| |_| |_|   |_| |_|    synced ring frequency
\  _______               _______
\ |       |_____________|       |_   synced envelope = reso
\  _____________________
\ |                     |_________   base frequency
\
\ These are combined by applying the envelope to the ring frequency
\ (AND) and mixing it with the base frequency (XOR). The result is
\ something looking like
\  _   _   _____________   _   _
\ | |_| |_|             |_| |_| |_   base frequency
\
\ Now, this 'looks' like a ringing square wave. If you look at the
\ spectrum of this, it has a peak at the ring frequency, and the width
\ of that peak can be influenced by the reso envelope.
    
    
\ CONTROL


    
\ n --  | retrigger the current sample for a couple of notes
\ : notes
\    note-sync bang
\    for note-sync next 
\    silence ;
    

\ the pattern language from synth.tex

\ pattern words depend on the variable 'time' which contains the
\ global time tick, i.e. 1/16 note count.
variable  time


macro
\ For patterns.  The '.' is the jump table separator and can be used directly.
: o  bang . ;   \ run 'bang' then exit jump table.

: pattern
    time @    \ behaviour depends on global 'time' variable
    #x0F and  \ to simplify, all patterns have length 16
    route ;

forth

\ whenever 'wicked' is called, the variable time is inspected (see the
\ 'pattern' macro above) to pick the right thing to do at that point:
\ either a 'bang' or nothing.
  
: wicked pattern  o  .  .  .  o  .  .  o  .  .  o  .  o  o  .  o
: funk   pattern  .  .  .  o  .  .  o  .  .  .  .  o  .  .  .  .
: drone  pattern  .  o  .  .  .  o  .  .  .  o  .  .  .  .  o  .    
    
    
\ n -- \ run the pattern sequencer for n notes
: pattern-sequencer
    0 time ! \ start with initialized time
    for
	\ the idea is that at each note instance, we select the
	\ current instrument, and then let a pattern decide wether to
	\ trigger it or not, depending on the current time

	\ select    \ pattern
	bassdrum    wicked
	snare       funk
	detune-pwm  drone
	
	time 1+!    \ advance time for the pattern words
	note-sync   \ our time base is notes
    next
    silence ;

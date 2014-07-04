\ sheepsint control module


\ CONTROL


\ OSCILLATOR PERIOD

\ Convention: All words starting with underscore consume and/or
\ produce 16bit values.
    
\ simple synth configs. note:
\   - need to disable interrupts during period update.
\   - the single byte access uses 'dup' to obtain #x101 *
\   - before writing, periods are negated, so we can work with unsigned values
\   - dup/drop trick to keep interrupts enabled one more cycle

\ need to limit period size to acceptable value to avoid rollovers or
\ simply to fast interrupt rates that cause lockups. require the period
\ to be > #xFF, meaning the high byte cannot be zero.


: clamp/inv   0 xor z? if drop drop 0 1 then  \ fall into 2inv
: 2inv        INDF0 comf d=reg #xff xor ;

macro
: per! | var |
    clamp/inv
    cli
    var 1 + ! dup
    var !
    sti drop ;
\     >m m-dup
\     clamp/inv cli
\     m> 1 + ! dup
\     m> ! sti drop ;

: per@ | var |
    var @
    var 1 + @
    2inv ;
    
\     >m m-dup
\     m> @ m> 1 + @
\     2inv ;
forth
  
: p0 dup : _p0 posc0l per! ;
: p1 dup : _p1 posc1l per! ;
: p2 dup : _p2 posc2l per! ;


: _p0@ posc0l per@ ;
: _p1@ posc1l per@ ; 
: _p2@ posc2l per@ ;




\ PRESET SYNTH PATCHES

\ The way to construct a synth byte is to start with the mixer number,
\ and then set bits using the 'bit' word.
    

macro
: bit 1 swap <<< or ;    \ inbyte n -- outbyte   | set bit number n
forth
    
: _square    
: square
    mix:xmod
    sync:0->1 bit
    sync:0->2 bit synth !

    -1 p1 -1 p2 ;  \ disabled.

: _xmod2    
: xmod2
    mix:xmod
    sync:0->2 bit synth !

    -1 p2 ;       \ disabled

: _xmod3    
: xmod3
    mix:xmod synth ! ;

: _rxmod    
: rxmod
    randbyte p0
    randbyte p1
    randbyte p2
    xmod3 ;


\ what about chord?   0->1->2 ?    

: _>r >r >r ;
: _r> r> r> ;    

: pwm dup
: _pwm
    xmod2
    _>r _p0@
    _r> _- _p1 ;  \ add modulation frequency


: _noise    
: noise
    mix:osc1
    osc1:noise bit synth !
    -1 p0  \ disabled
    -1 p2
    1 p1 ; \ +- 8 kHz

staapl pic18/double-math  \ 16 bit math routines
staapl pic18/double-hacks \ some optimized ones
    
: reso
    mix:reso
    sync:0->1 bit
    sync:0->2 bit synth !

    _p0@ 
    _>> _dup _p2     \ half max reso
    _>>      _p1 ;   \ reso freq 4x
    

: silence 0 synth ! ;    

\ NOTE
    
\ A4 above [middle] C4 is 440Hz
\ this makes C5 = 440 2 sqrt sqrt *

\ the lowest frequency that can be obtained at 2 MIPS with 2^16
\ subdivision is 30.517578 Hz, which is a little lower than C1 at
\ 32.703194 so the magic numbers are based on

\ C1 = 61156

\ there are 2 words used here. 'note' computes a note, while 'octave'
\ sets the current octave. the word 'bass-note' computes the lowest
\ octave (starting from C1)

\ load metaprogramming stuff in badnop namespace    
\ [ badnop (synth meta) use ]


\ require pic18/geo.ss  \ geo-seq macro

macro
: geo-seq ' ,, compile-geo-seq ;
forth

: bass->period
    23 min _table   \ 2 octaves, more convenient..
    
    61156      \ start
    dup 4 /    \ endx
    24         \ nb of items
    geo-seq    \ compile geometric sequence



variable oct
  
: _note
    drop \ drop high byte
: note
    bass->period
    oct @ nz? if
	for _>> next
    else
	drop
    then
    ;

: _octave
    drop \ ignore high byte
: octave
    oct ! ;

\ bound to some oscillator
: note0 note _p0 ;
: note1 note _p1 ;
: note2 note _p2 ;

\ 8 bit fetch for simple effects
: p0@  _p0@ nip ;
: p1@  _p1@ nip ;
: p2@  _p2@ nip ;

    
    
\ CONTROL TIMER

\ the fixed rate will increment a 32bit 'tick' counter once every
\ cycle. this counter can be used as a timer for the control rate
\ synchronization.

\ the fixed interrupt (with cpu at 2 MIPS) runs at 7812 Hz, and
\ increments a counter that can be used for longer time
\ intervals. From this we'll derive 2 standard sync timers: control
\ rate and note rate, at about 200 Hz and 8 Hz

\ to sync, first wait for a bit to go low, then wait for it to go
\ high. the different tick bits give

\ tick0 0   3906   Hz
\       4    244   Hz
\ tick1 1      7.6 Hz


\ FIXME: rewrite with lex params    
    
macro
: tickbit | bit |
    tick0 bit 3 >>> +   \ tick + byte address
    bit 7 and ;         \ bit address

: sync-tick | bit |
    begin bit tickbit low? until
    begin bit tickbit high? until ;
	
forth

\ these have changed + it's better to use tasks
\ : wait-note    9 sync-tick ;  \ 7.8 Hz
\ : wait-control 4 sync-tick ;  \ 244 Hz
    


\ TRANSIENTS + EFFECTS

\ words here will sync to control rate. transients that use
\ oscillators should save the frequencies in addition to synth config.

: s>r  synth @ >r ;
: r>s  r> synth ! ;

: p0>r _p0@ _>r ;  : r>p0 _r> _p0 ;
: p1>r _p1@ _>r ;  : r>p1 _r> _p1 ;
: p2>r _p2@ _>r ;  : r>p2 _r> _p2 ;

\ transient noise burst, used for hihat / snare
: burst
    s>r noise
    for 2 sync-tick next
    r>s ;
    
\ PANEL

load ad.f
\ load synth-panel.f

macro
: andor! | reg | reg and! reg or! ;
    \ >m m-dup m> and! m> or! ;

: synth-bits! | mask |
    mask and mask #xFF xor synth andor! ;
    
: rot<<3 rot<< rot<< rot<< ;    
    
forth  


  
: param

    \ first 3 pots set period
    
    0 ad@ p0
    1 ad@ p1 
    2 ad@ p2

    \ pot bits are XOR FF because they are upside down.
    
    \ pot nb 3 is [ noise:1 synth:2 ignored:5 ] 
    3 ad@ #xFF xor dup

    \ set noise bit synth:7
    #b10000000 synth-bits!

    \ set mixer algo bits synth:1-0
    rot<<3
    #b00000011 synth-bits!
    
    \ pot nb 4 sets sync bits [ sync: 3 ignored: 7 ]

    4 ad@ #xFF xor rot>>
    #b01110000 synth-bits!

    ;


staapl pic18/route
staapl pic18/compose-macro

\ USB MIDI connected to EP 3

: midi-EP 3 ;

\ USB is named from pov. of host.  For midi words below, we use a
\ slightly less awkward device-centered view.
    
: midi-out-begin midi-EP 4 IN-begin ;
: midi-out-end   IN-end midi-EP IN-flush ;

: midi-in-begin  midi-EP 4 OUT-begin ;
: midi-in-end    OUT-end ;
    

\ FIXME: connect any panel pots to send out CC    
: note-on \ note --
    midi-out-begin
        #x09 >a  \ cable, class
        #x90 >a  \ note on channel 0
             >a  \ note value
        127  >a  \ velocity
    midi-out-end ;
    
: note-off \ note --
    midi-out-begin
        #x08 >a  \ cable, class
        #x80 >a  \ note on channel 0
             >a  \ note value
        127  >a  \ velocity
    midi-out-end ;


variable midi-cin \ USB only
variable midi-byte0 : m0 midi-byte0 @ ;
variable midi-byte1 : m1 midi-byte1 @ ;
variable midi-byte2 : m2 midi-byte2 @ ;
    
: midi-in \ -- class
    midi-in-begin
        a> midi-cin !
        a> midi-byte0 !
        a> midi-byte1 !
        a> midi-byte2 !
    midi-in-end ;

\ : note-in  begin midi-in m0 #x90 = until m1 ;
        

load midi-arp.f
\ load debug.f

variable pitch-lo  \ low  byte from midi, shifted left one bit
variable pitch-hi  \ high byte from midi

variable nrpn-addr-hi
variable nrpn-addr-lo
variable nrpn-val-hi
variable nrpn-val-lo
: nrpn-val15 \ -- lo hi | convert to 15-bit value
    nrpn-val-lo @ rot<<
    nrpn-val-hi @ ;
  
\ NRPN CC
: CC63 nrpn-addr-hi ! ; 
: CC62 nrpn-addr-lo ! ; 
: CC06 nrpn-val-hi ! ;
: CC26 nrpn-val-lo !
    \ This is optional in midi spec, though we require it to start
    \ transaction
    commit-nrpn ; 

: _dup over over ;
: _drop 2drop ;    

macro \ trampoline for far away jump addresses
: .. i/c . ;
forth  
    
\ low-level synth parameters are available through NRPN.
: commit-nrpn
    psps
    nrpn-addr-hi @ 0 = not if ; then \ ignore
    nrpn-val15
    nrpn-addr-lo @
    ` nrpn: .sym ts
    1 - 4 min
    ts
    route
        [ _p0    ] ..  \ 1
        [ _p1    ] ..  \ 2
        [ _p2    ] ..  \ 3
        [ _synth ] ..  \ 4
        _drop ;  \ ignore rest


        
\ Guard word: aborts call if condition is not met.
: ~chan m0 #x0F and 0 = if ; then xdrop ;
  
\ Handle USB MIDI Code Index Number
: CIN9 ~chan  m1 notes-add    play-last ;
: CIN8 ~chan  m1 notes-remove play-last ;
: CINB ~chan  CC  ;
: CINE ~chan  m2 << pitch-lo ! m2 pitch-hi ! ; 

: .synth ` synth: .sym  synth @ px cr ;
    
: pm12  m1 px m2 px cr ; \ pitchbend

\ During silence we need to save synth config.
variable synth-save    

    
\ from midi-arp.f : get most recently pressed active key    
: play-last
    notes-last #xFF = if silence ; then
    notes-last midi note0
    \ square
: restore-synth    
    synth-save @ synth !
    ;

\ Controllers should set meaningful high level values.  The synth
\ engine is already fully controllable through NRPN.
    
: CC57
: CC58 
: CC59 
: CC5A 
: CC55 
: ____ drop ` ignore:cc: .sym pm12 ;
    
\ controller jump table.  this is sparse but we have plenty of room in Flash
: CC 
    m2 m1 #x7F and route
        \  0      1      2      3      4      5      6      7      8      9      A      B      C      D      E      F
        ____ . ____ . ____ . ____ . ____ . ____ . CC06 . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . \ 0
        ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . \ 1
        ____ . ____ . ____ . ____ . ____ . ____ . CC26 . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . \ 2 
        ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . \ 3
        ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . \ 4
        ____ . ____ . ____ . ____ . ____ . CC57 . ____ . CC57 . CC58 . CC59 . CC5A . ____ . ____ . ____ . ____ . ____ . \ 5
        ____ . ____ . CC62 . CC63 . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . \ 6
        ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ ; \ 7
        
: midi-poll-once
    \ psps
    midi-in midi-cin @ #x0F and route
           .      .      .      .
           .      .      .      .
      CIN8 . CIN9 .      . CINB .
           .      . CINE .      ;

: midi-poll begin midi-poll-once again
        
: go
    square synth @ synth-save ! silence
    init-notes engine-on midi-poll ;




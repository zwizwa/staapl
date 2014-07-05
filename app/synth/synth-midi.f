staapl pic18/route

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


variable midi-0
variable midi-1 : m1 midi-1 @ ;
variable midi-2 : m2 midi-2 @ ;
variable midi-3 : m3 midi-3 @ ;
    
: midi-in \ -- class
    midi-in-begin
        a> midi-0 !
        a> midi-1 !
        a> midi-2 !
        a> midi-3 !
    midi-in-end ;

\ : note-in  begin midi-in m1 #x90 = until m2 ;
        

load midi-arp.f
\ load debug.f

variable pitch-lo  \ low  byte from midi, shifted left one bit
variable pitch-hi  \ high byte from midi

\ NRPN CC
\ #x63 ADDR HI
\ #x62 ADDR LO
\ #x06 VAL HI
\ 3x26 VAL LO (optional)
  
\ Guard word: aborts call if condition is not met.
: ~chan m1 #x0F and 0 = if ; then xdrop ;
  
\ Handle USB MIDI Code Index Number
: CIN9 ~chan  m2 notes-add    play-last ;
: CIN8 ~chan  m2 notes-remove play-last ;
: CINB ~chan  CC ; \ ` c: .sym pm23 ;
: CINE ~chan  m2 << pitch-lo ! m3 pitch-hi ! ; \ ` p: .sym pm23 ;

: pm23  m2 px m3 px cr ; \ pitchbend


\ from midi-arp.f : get most recently pressed active key    
: play-last
    notes-last #xFF = if silence ; then
    notes-last midi note0 square ;


\ controller jump table.  this is sparse but we have plenty of room in Flash
: CC57 m3
    \ Controller sets Noise, Algo, Sync:
    \ 0 N A1 A0 | S2 S1 S0 x 
    \ global synth algo config
    \ set noise bit synth:7
    dup
    rot<<
    #b10000000 synth-bits!

    \ set mixer algo bits synth:1-0
    dup
    rot>>4
    #b00000011 synth-bits!
    
    \ pot nb 4 sets sync bits [ sync: 3 ignored: 7 ]
    >>
    rot>>4
    #b01110000 synth-bits! ;
    

    

    ;
: CC58 ;
: CC59 ;
: CC5A ;
: CC55 ;

    
: ____ ` ignore:cc: .sym pm23 ;
: CC 
    m2 #x7F and route
        \  0      1      2      3      4      5      6      7      8      9      A      B      C      D      E      F
        ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . \ 0
        ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . \ 1
        ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . \ 2 
        ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . \ 3
        ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . \ 4
        ____ . ____ . ____ . ____ . ____ . CC57 . ____ . CC57 . CC58 . CC59 . CC5A . ____ . ____ . ____ . ____ . ____ . \ 5
        ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . \ 6
        ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ . ____ ; \ 7
        
: midi-poll-once
    \ psps
    midi-in midi-0 @ #x0F and route
           .      .      .      .
           .      .      .      .
      CIN8 . CIN9 .      . CINB .
           .      . CINE .      ;

: midi-poll begin midi-poll-once again
        
: go init-notes engine-on midi-poll ;




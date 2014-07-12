staapl pic18/route
staapl pic18/compose-macro
staapl pic18/stdin
staapl pic18/cond
staapl pic18/afregs
staapl pic18/double-math
staapl pic18/vector

\ https://ccrma.stanford.edu/~craig/articles/linuxmidi/misc/essenmidi.html
\ http://www.nortonmusic.com/midi_cc.html

variable midi-byte0 : m0 midi-byte0 @ ;
variable midi-byte1 : m1 midi-byte1 @ ;
variable midi-byte2 : m2 midi-byte2 @ ;
    
2variable pitch-mod  \ low  byte from midi, shifted left one bit
2variable mod1       

  
\ During silence we need to save synth config.
variable synth-save    

\ Main pitch
variable period
 

: m-interpret \ --
    m0-cmd route
        8x . 9x .    . Bx .
        Cx .    . Ex .    ;

: m0-cmd \ 0-7 | maps 9x-Fx to 0-7 for route command
    m0 rot>>4 8 - 7 and ;
        
\ Guard: aborts caller if incorrect channel.
: ~chan m0 #x0F and 0 = if ; then xdrop ;
    
: 9x ~chan  m1 notes-add    play-last ;
: 8x ~chan  m1 notes-remove play-last ;
: Bx ~chan  continuous-controller  ;
: Cx ~chan  m1 program-change ;    
: Ex ~chan  m1 m2 pitch-mod 2! ;
    

: play-last
    \ print-notes
    notes-last #xFF = if silence ; then
    notes-last midi-note
    _dup period 2!
    _p0
    \ square
: restore-synth    
    synth-save @ synth !
    ;

\ Controllers should set meaningful high level values.  The synth
\ engine is already fully controllable through NRPN.
    
: CC57 midi-note mod1 2! ;
: CC58 
: CC59 
: CC5A 
: CC55 
: ____ drop ` ignore:cc: .sym pm12 ;
    
\ jump table is sparse but we have plenty of room in Flash
: continuous-controller
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


\ Eventually it might be interesting to find out a generic patch
\ format, but for now these are best pre-defined.
        
: program-change \ program --
    2 min route
        square .
        ;
    

\ USB PACKET / STREAM

\ usb>m | For USB MIDI, data is packetized already.
\ i>m   | For MIDI streams, the messages are not segmented so need
\ "smart parsing" if we want to get to a message payload before the
\ next command byte arrives.

\ ( It's always possible to segment based on command bytes, but that
\ introduces one byte delay.  This might actually not be a huge
\ problem if the device also sends tick messages. )



\ STREAM        



\ For MIDI cable, this can be called in a polling loop with i> set to
\ the UART input.  For midi USB this is called once per loop with i>
\ set to a> in the buffer.
    
\ Note this only works properly with d=i if the message is
\ well-formed.

\ NOT TESTED       
: i>m \ --
    \ Postcondition is valid midi frame in midi-byte0/1/2.  Replace a
    \ non-command byte with dummy active sensing byte, effectively
    \ ignoring it.
    i> 1st 7 low? if drop #xFE then
    midi-byte0 !
    m0-cmd route
        i>m12 . i>m12 . i>m12 . i>m12 . \ 8 9 A B
        i>m12 . i>m1  . i>m12 .       ; \ C D E F

: i>m1       i> midi-byte1 ! ;
: i>m12 i>m1 i> midi-byte2 ! ;



        


\ USB PACKET 


\ USB is named from pov. of host.  For midi words below, we use a
\ slightly less awkward device-centered view.
    
\ : usb-midi-out-begin midi-EP 4 IN-begin ;
\ : usb-midi-out-end   IN-end midi-EP IN-flush ;

: usb-midi-in-begin  midi-EP 4 OUT-begin ;
: usb-midi-in-end    OUT-end ;
    

\ TESTED.  Connect any panel pots to send out CC    
\ : note-on>usb \ note --
\     usb-midi-out-begin
\         #x09 >a  \ cable, class
\         #x90 >a  \ note on channel 0
\              >a  \ note value
\         127  >a  \ velocity
\     usb-midi-out-end ;
    
\ : note-off>usb \ note --
\     usb-midi-out-begin
\         #x08 >a  \ cable, class
\         #x80 >a  \ note on channel 0
\              >a  \ note value
\         127  >a  \ velocity
\     usb-midi-out-end ;

: usb>m \ -- 
    usb-midi-in-begin
        a> drop  \ we don't use USB Code Index Number
        a> midi-byte0 !
        a> midi-byte1 !
        a> midi-byte2 !
    usb-midi-in-end ;
        

: midi-poll-once
    usb>m
    \ m0 #xF8 = not if m-print then
    \ i>r midi-uart>i i>m r>i  \ or something like that..
    m-interpret ;

: init-synth
    square synth @ synth-save ! silence
    init-notes engine-on ;
    

macro
: usb-midi-ready? midi-EP OUTrem 0 = not ;  
forth

: poll-midi usb-midi-ready? not if ; then midi-poll-once ;


\ DEBUG: comment-out in standalone version
\ : a!midi-bytes midi-byte0 0 a!! ;
\ : m-clear a!midi-bytes 3 for 0 >a next ;
\ : m-print a>r a!midi-bytes 3 for a> px next cr r>a ;
\ : m-test 2 1 #x90 d>i i>m ;    
\ : .synth ` synth: .sym  synth @ px cr ;
: pm12  m1 px m2 px cr ;
    
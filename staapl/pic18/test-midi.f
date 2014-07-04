
\ Midi connected to EP 3

: midi-EP 3 ;

\ USB is named from pov. of host.  For midi words below, we use a
\ slightly less awkward device-centered view.
    
: midi-out-begin midi-EP 4 IN-begin ;
: midi-out-end   IN-end midi-EP IN-flush ;

: midi-in-begin  midi-EP 4 OUT-begin ;
: midi-in-end    OUT-end ;
    
    
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
variable midi-1
variable midi-2
variable midi-3  
    
: midi-in \ -- class
    midi-in-begin
        a> midi-0 !
        a> midi-1 !
        a> midi-2 !
        a> midi-3 !
    midi-in-end ;

: note-in begin midi-in midi-1 @ #x90 =? until midi-2 @ ;
        
    
    
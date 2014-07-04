
\ Midi connected to EP 3
: midi-begin 3 IN-begin ;
: midi-end   IN-end 3 IN-flush ;   

    
: note-on \ note --
    midi-begin
    #x09 >a  \ cable, class
    #x90 >a  \ note on channel 0
         >a  \ note value
    127  >a  \ velocity
    midi-end ;
    
: note-off \ note --
    midi-begin
    #x08 >a  \ cable, class
    #x80 >a  \ note on channel 0
         >a  \ note value
    127  >a  \ velocity
    midi-end ;
    
    
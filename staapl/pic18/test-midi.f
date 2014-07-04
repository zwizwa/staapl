
\ Midi connected to EP 3
: >midi      3 >IN ;
: midi-flush 3 IN-flush ;


    
: note-on \ note --
    #x09 >midi  \ cable, class
    #x90 >midi  \ note on channel 0
         >midi  \ note value
    127  >midi  \ velocity
    midi-flush
    ;
    
: note-off \ note --
    #x08 >midi  \ cable, class
    #x80 >midi  \ note on channel 0
         >midi  \ note value
    127  >midi  \ velocity
    midi-flush
    ;
    
    
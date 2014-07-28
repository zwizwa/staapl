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
        

: usb-midi-once
    usb>m
    m-interpret ;

    
macro
: usb-midi-ready? midi-EP OUTrem 0 = not ;  
forth

: poll-usb-midi usb-midi-ready? if usb-midi-once then ;
    
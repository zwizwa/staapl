staapl pic18/serial

\ PIC18F4550:
\ TX = RC6
\ RX = RC7

\ 6N136 midi in:
\ http://www.natrium42.com/wiki/File:Midi-schematic.gif

macro
: midi-ready? async.rx-ready? ;
forth

: init-midi 31250 fosc init-serial ;
: >midi begin async.tx-ready? until async.>tx ;
: midi> begin async.rx-ready? until async.rx> ;

\ Clock check
\ : midi-clockcheck init-midi begin #x55 >midi again
    
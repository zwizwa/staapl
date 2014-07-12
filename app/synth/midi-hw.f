staapl pic18/serial

\ PIC18F4550:
\ TX = RC6
\ RX = RC7


: init-hwmidi 31250 fosc init-serial ;
: >hwmidi begin async.tx-ready? until async.>tx ;
: hwmidi> begin async.rx-ready? until async.rx> ;

\ Clock check
: hwmidi-clockcheck init-hwmidi begin #x55 >hwmidi again
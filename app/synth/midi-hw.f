\ staapl pic18/serial
staapl pic18/serial-debug
staapl pic18/compose-macro
staapl pic18/vector
staapl pic18/stdin
staapl pic18/afregs
staapl pic18/cond

\ PIC18F4550:
\ TX = RC6
\ RX = RC7

\ 6N136 midi in:
\ http://www.natrium42.com/wiki/File:Midi-schematic.gif

\ 16 byte buffer
variable midi-write  
variable midi-read
macro
: midi-buf #xF0 1 ;

\ Generic circular buffer hole macro
: bitmask | bits | 1 bits <<< 1 - ;
: a!circle+ | lo hi ptr bits |
    lo hi a!!
    ptr @ bits bitmask and al +!
    ptr 1+! ;
    
: midi-size   midi-write @ midi-read @ - ;
: midi-empty? midi-size 0 = ;
: midi-ready? midi-empty? not ;
: midi-full?  midi-size 16 = ; 

forth

: >midi-buf  a>r midi-buf midi-write 4 a!circle+ >a r>a ;
: midi-buf>  a>r midi-buf midi-read  4 a!circle+ a> r>a ;   
    

: service-midi \ included in low-pri isr
    async.rx-ready? if
        \ FIXME: this can't busy-wait, but handle errors!
        RCREG @ 
        midi-full? if
            drop
        else
            >midi-buf
        then
    then ;
: midi>
    begin midi-ready? until
    midi-buf> ;
    
forth

: init-midi-buf
    0 midi-write !
    0 midi-read ! ;
  
: init-midi
    0 midi-write !
    0 midi-read !
    31250 fosc init-serial
    \ serial-enable-isr-lo
    IPR1 RCIP low  \ low pri
    PIE1 RCIE high \ enable
    ;
    
: i=midi stdin -> midi> ; \ dup px ; 
    
: midi-once
    i>r
    i=midi i>m
    m-interpret
    r>i ;

: poll-hw-midi midi-ready? if midi-once then ;
    
\ Clock check
\ : midi-clockcheck init-midi begin #x55 >midi again


\ doesnt work : the printing messes up the UART timing
\ : dump-midi init-midi begin midi> 1st 7 high? if cr then px again

\ this one does work    
\ : a!debug 0 7 a!! ;
\ : getsome init-midi a!debug #x100 for midi> >a next ;
\ : dumpsome a!debug #x100 for a> px next ;
    
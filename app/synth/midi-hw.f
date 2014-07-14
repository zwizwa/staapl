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
: midi-bufspec #xE0 1 5 ; \ 32 bytes at 1E0

\ Generic circular buffer hole macro
: bitmask | bits | 1 bits <<< 1 - ;
: a!circle+ | ptr lo hi logsize |
    lo hi a!!
    ptr @ logsize bitmask and al +!
    ptr 1+! ;
: a!midibuf+ midi-bufspec a!circle+ ;    
    
: midi-empty? midi-size 0 = ;
: midi-ready? midi-empty? not ;
: midi-full?  midi-size 32 = ; 

forth
: midi-size   midi-write @ midi-read @ - ;
: >midi-buf   a[ midi-write a!midibuf+ >a ]a ;
: midi-buf>   a[ midi-read  a!midibuf+ a> ]a ;   
    

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
    
: init-midi-buf
    0 midi-write !
    0 midi-read ! ;
  
: init-midi
    init-midi-buf
    31250 fosc init-serial
    \ serial-enable-isr-lo
    IPR1 RCIP low  \ low pri
    PIE1 RCIE high \ enable
    ;

    
: midi-once
    midi>m
    m-interpret ;

: poll-hw-midi midi-ready? if midi-once then ;
    
\ Clock check
\ : midi-clockcheck init-midi begin #x55 >midi again


\ doesnt work : the printing messes up the UART timing
\ : dump-midi init-midi begin midi> 1st 7 high? if cr then px again

\ this one does work    
\ : a!debug 0 7 a!! ;
\ : getsome init-midi a!debug #x100 for midi> >a next ;
\ : dumpsome a!debug #x100 for a> px next ;
    
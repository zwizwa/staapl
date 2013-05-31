#lang planet zwizwa/staapl/pic18 \ -*- forth -*-
provide-all

staapl pic18/serial
load p18f2550-const.f

\ [PIC,xxx] 18f2550 data sheet (Microchip DS39632C)
\ [USB,xxx] the usb 1.1 specification

\
\ usb device state
variable   state

macro
: POWERED 0 ;
: DEFAULT 1 ;
: ADDRESS 2 ;
: CONFIG 3 ;
forth

\ **** LOWLEVEL ****
  
: init-usb
    0 UIE !   \ mask all USB interrupts
    0 UIR !   \ clear all interrupt flags
    
    \ #x10 UCFG ! \ int pullup + transceiver, low speed, no pingpong
    #x14 UCFG ! \ int pullup + transceiver, full speed, no pingpong
    #x08 UCON ! \ USBEN high: needs to be done last [PIC,164]
 
    POWERED state !

    begin UCON SE0 high? until \ wait for single ended zero on bus
    
    \ something with config i dont get
;

: debug emit ;    

macro
: usbi? | F | UIR F high? ;  
forth
    
: service-usb
    \ [PIC,178]
    UERRIF  usbi? if 101 debug             ; then
    SOFIF   usbi? if           soframe     ; then
    IDLEIF  usbi? if 103 debug idle        ; then
    ACTVIF  usbi? if 104 debug activity    ; then
    STALLIF usbi? if 105 debug stall       ; then
    URSTIF  usbi? if 106 debug usbreset    ; then
    TRNIF   usbi? if 107 debug transaction ; then
;

: activity UIR ACTVIF low ; \ UCON SUSPND low ;     \ bus activity detected
: soframe  UIR SOFIF low ;                      \ START-OF-FRAME token received
: stall    UIR STALLIF low ;                    \ STALL handshake sent
: idle     UIR IDLEIF low ; \ UCON SUSPND high ;    \ idle condition detected

    \ Using proper words to abstract buffer management.  Macros are too
\ hard to use here.
    

\ Buffer descriptor registers at #x400.  Not in access bank so these
\ need either bank select or indirect addressing.  Going for the latter.

\ These words select context (in a register) and incrementally buld state.

\ Current object selection and reference.  
: EP << << <<
: BD  al ! 4 ah ! ;
: OUT ;
: IN  4 al +! ;    
: BD@   @i ;  \ index -- val 
: BD!   !i ;  \ val index --

: STAT 0 ;
: CNT 1 ;
: ADRL 2 ;
: ADDRH 3 ;    

\ Buffer control    
: MCU STAT BD@ #x3F and STAT BD! ;  \ UOWN = MCU
: SIE STAT BD@ #x80 or  STAT BD! ;  \ UOWN = SIE
: DATA-toggle STAT BD@ #x40 xor STAT BD! ;
: DATA1 STAT BD@ #x40 or STAT BD! ;    
: SIZE CNT BD! ;
    
    
macro
: buf-OUT #x480 ;
: buf-IN  #x4C0 ;  
forth
    
: init-BD
    template
    #x88 , 64 , buf-OUT ,,
    #x08 , 64 , buf-IN ,,
    
\ valid usb reset occured
: usbreset

    POWERED state !

    \ clear out the USTAT FIFO and EP control regs
    4 for UIR TRNIF low next        
    UEP0 >block clear-block
    
    \ init buffer descriptors
    init-BD #x400 >block init-block

    0 UADDR !              \ set USB address to 0
    #x16 UEP0 !            \ control: in, out, setup + handshake enabled + stall off
    #xff UEIE !            \ enable all error interrupts

    DEFAULT state !
    UIR URSTIF low ;



\ *** SETUP TRANSACTIONS ***

    
\ Mask out pingpong bit to get the index of the current buffer
\ descriptor (the one that triggered the transaction interrupt).
: trans-bd 
    USTAT @ #x7C and ; 
: trans-ep
    trans-bd >> >> >> ;
: trans-pid
    trans-bd BD
    STAT BD@ >> >> #xF and ;

: transaction
    trans-pid dup debug
    \ usb core gives only IN OUT STATUS,
    \ so need only top 2 bits:
    >> >> route
        trans-OUT . .  \ 0001
        trans-IN .     \ 1001
        trans-SETUP ;  \ 1101

: OUT-ACK
    64  CNT  BD!
    SIE STAT BD! ;
        
\ Acknowledge reception.   
: trans-ack
    0 EP OUT  64 SIZE SIE  \ return OUT buffer to SIE
    0 EP IN   MCU          \ return IN buffer to MCU
    UCON PKTDIS low \ enable packet transfer (disabled by SIE after SETUP pkt)
    UIR TRNIF low   \ signal we're done handling the packet
    ;

\ SETUP packets arrive in the EP0 OUT buffer.
: SETUP>a \ i --
    buf-OUT + al !
    buf-OUT 8 >>> ah ! ;
: SETUP@ \ i -- v
    SETUP>a @a+ ;

\ SETUP packet queries.

\ Note that these are valid until trans-ack, so for some we need to
\ copy items to the stack or somewhere else before continuing.
    
: SETUP:bRequest      1 SETUP@ ;     \ [USB,190]
: SETUP:wValue        2 SETUP@ ;
: SETUP:wValueHigh    3 SETUP@ ;

\ Dispatch request.
: trans-SETUP
    SETUP:bRequest dup debug
    route
        R00 . R01 .     . R03 .
            . R05 . R06 . R07 .
        R08 . R09 . R0A . R0B ;
        
            
: R00 : GET_STATUS ;
: R01 : CLEAR_FEATURE ;
: R03 : SET_FEATURE ;
: R05 : SET_ADDRESS ;
    
: R06
: GET_DESCRIPTOR  \ [USB,189]
    SETUP:wValue            \ index
    SETUP:wValueHigh 7 and  \ type
    trans-ack dup debug
    route
        . D01 . D02 . D03 .
        D04 . D05 . . ;

macro
: lohi dup 8 >>> ;
forth
        
\ Copy data from Flash to IN buffer RAM and return the number of bytes
\ copied.
: buf-IN>a
    buf-IN lohi a!! ;
: copy-IN
    f!! buf-IN>a
    @f+ dup >r
    for @f+ !a+ next
    r> ;

\ Transfer a stored Flash message as a reply.    
: ROM-reply \ lo hi
    copy-IN \ n
    dup debug
: reply \ n --
    0 EP IN
    SIZE
    DATA-toggle
    SIE ;
    
\ index --        
: D01 : DEVICE 
    device-descriptor \ lo hi
    ROM-reply ;
    
: D02 : CONFIGURATION ;
: D03 : STRING ;
: D04 : INTERFACE ;
: D05 : ENDPOINT ;    
    
        
    
: R07 : SET_DESCRIPTOR ;
: R08 : GET_CONFIGURATION ;

: R09
: SET_CONFIGURATION
    trans-ack  \ FIXME: check this..
    0 reply ;

: R0A : GET_INTERFACE ;
: R0B : SET_INTERFACE ;
 

\ **** IO TRANSACTIONS ****
    

: trans-IN ;
: trans-OUT ;


    

: test
    init-usb
    begin service-usb again
    ;
    


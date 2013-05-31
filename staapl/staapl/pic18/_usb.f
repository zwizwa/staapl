\ -*- forth -*-

\ usb driver for pic18f2550

\ references:
\ [PIC,xxx] 18f2550 data sheet (Microchip DS39632C)
\ [USB,xxx] the usb 1.1 specification

\ IMPORTANT: this code uses indexed addressing mode for access bank
\ addresses 00-5F. this requires the XINST bit in CONFIG4L to be set

	
\ misc notes
\ * naming IN/OUT is from the host's point of view
\ * control packets are 8 bytes long for low speed, hi speed:8,16,32,64
\ * 3 kinds of transactions IN/OUT/SETUP
\ * a control transfer:
\   - a SETUP transaction
\   - optionally an IN/OUT transaction
\   - STATUS transaction


staapl pic18/shift
staapl pic18/interpreter
staapl pic18/route


\ Scheme module that generates the descriptor tables.
\ staapl pic18/usb-cdc



\ usb device state
variable   state
variable   endpoint        \ current endpoint
variable   device-address  \ current usb device address


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

: service-usb
    \ [PIC,178]
    UIR UERRIF  high? if 1 debug error ; then
    UIR SOFIF   high? if         soframe ; then
    UIR IDLEIF  high? if         idle ; then
    UIR ACTVIF  high? if         activity ; then
    UIR STALLIF high? if 5 debug stall ; then
    UIR URSTIF  high? if 6 debug usbreset ; then
    UIR TRNIF   high? if 7 debug transaction ; then
;



\ buffer descriptor fields, using relative addressing.  there are no
\ name spaces, so using a dot to indicate it's a structure member.

macro
: BD.STAT 0 ;
: BD.CNT 1 ;
: BD.ADRL 2 ;
: BD.ADRH 3 ;

\ setup data record
: SETUP.bmRequestType 0 ;
: SETUP.bRequest 1 ;
: SETUP.wValue 2 ;
: SETUP.wValueHigh 3 ;
: SETUP.wIndex 4 ;
: SETUP.wIndexHigh 5 ;
: SETUP.wLength 6 ;
: SETUP.wLengthHigh 7 ;
    
: POWERED 0 ;
: DEFAULT 1 ;
: ADDRESS 2 ;
: CONFIG 3 ;

\ usb buffer location
: usb-bufh 4  ;
: usb-bufl-OUT #x80 ; \ now reserving 64 bytes ;
: usb-bufl-IN #xC0   ;

forth

\ request types
\ 0xFF NO_REQUEST
\ 0x00 GET_STATUS
\ 0x01 


macro
  
\ *WORD* means the current object context has changed: all literal
\ addresses below #x60 are relative indexes.

: EPn-OUT    3 <<< ;
: EPn-IN     EPn-OUT 4 + ;    

\ ( reladdr -- ) set object to buffer descriptor in bank 4
: *BD*       4 a!! ;
: *EP0-OUT*  0 EPn-OUT *BD* ;
: *EP0-IN*   0 EPn-IN  *BD* ;

forth




  
\ valid usb reset occured
: usbreset

    POWERED state !
    
    4 for UIR TRNIF low next        \ clear out the USTAT FIFO
    UEP0 #x0F a!! 16 for 0 !a+ next \ clear EP control regs

    *EP0-OUT*
      64           BD.CNT !   \ 8 is max for low speed, 64 for high
      usb-bufh     BD.ADRH !
      usb-bufl-OUT BD.ADRL !
      #x88         BD.STAT !  \ set UOWN, so USB can write 

    *EP0-IN*
      64          BD.CNT !       
      usb-bufh    BD.ADRH !
      usb-bufl-IN BD.ADRL !
      #x08        BD.STAT !  \ clear UOWN, so MCU can write
    
    0 UADDR !              \ set USB address to 0
    0 UIR !                \ clear all usb interrupt flags    
    #x16 UEP0 !            \ control: in, out, setup + handshake enabled + stall off
    #xff UEIE !            \ enable all error interrupts

    DEFAULT state ! ;

\ layer 0
: error    0 UEIR ! ;                           \ usb error
: soframe  UIR SOFIF low ;                      \ START-OF-FRAME token received
: idle     UIR IDLEIF low UCON SUSPND high ;    \ idle condition detected
: activity UIR ACTVIF low UCON SUSPND low ;     \ bus activity detected
: stall    UIR STALLIF low ;                    \ STALL handshake sent


macro    
\ : debug transmit ;
: debug drop ;
forth

\ ( offset -- )  \ folow pointer in current object  
: *@* al +! @a+ @a+ a!! ;



\ handle tokens    
: OUT   #x10 debug ;


: IN
    \ used for completing requests, currently only used after SET_ADDRESS
    device-address @ UADDR !
    ack-transaction           \ necessary ?
    ;



\ SETUP requests

: R00 : GET_STATUS ;
: R01 : CLEAR_FEATURE ;
: R03 : SET_FEATURE ;


\ the tricky thing with this request is that the operation (set the
\ address, here this means storing UADDR) needs to be done after the
\ transaction is complete.
    
: R05 : SET_ADDRESS
    SETUP.wValue @   \ get the address
    device-address !

    ack-transaction  \ done interpreting SETUP

    \ send an empty reply back
    *EP0-IN*
      0    BD.CNT !     \ empty packet
      #xC8 BD.STAT !    \ DATA1, set UOWN
    
    ;


\ sending descriptors

\ ( n -- ) copy a block of n bytes from flash to ram. this advances
\ the pointers.
    
: n@f!a  for @f+ !a+ next ;

    
: send-descriptor ;

\ save/restore current object to retain stack
: *push* ah @ >r al @ >r ;   \ --
: *pop*  r> al ! r> ah ! ;   \ --


\ a = current ram object = reply buffer BD
\ f = current flash object = constant data

\ : f-reply \ n --
\     dup BD.CNT !       \ store size
\     *push*
\        BD.ADRL *@*     \ data is current object
\        n@f!a           \ transfer bytes to buffer
\     *pop*
\     #xC8 BD.STAT ! ;   \ DATA1, set UOWN
  

    \ TODO:
    \ - check reset
    \ - check ProcessSetupToken in lab1
    \ - crossref the C code


\ ENDPOINT OPS


macro
: device-descriptor ;  
forth
    
    
: DEVICE
    \ -3 debug
    
    device-descriptor  \ sets current flash object
    @f+                \ get size of device descriptor record
    \ dup debug
    
    *EP0-IN*           \ current object = reply BD
      dup BD.CNT !     \ store in BD

    BD.ADRL *@*        \ current ram object = data buffer
      n@f!a            \ transfer device descriptor bytes from flash
    
    *EP0-IN*
      BD.STAT @          \ [PIC,173]
      #x40 xor           \ toggle DATA01 (bit is never written by SIE, so is valid)
      #x40 and           \ clear PID bits 
      #x88 or            \ set UOWN and DTS bits
      BD.STAT !          \ go
    
    \ -1 debug 
    
    ;

    \ call Descriptor
    \ call SendDescriptorPacket
    \ TODO: copy from flash -> buffer

    
: CONFIGURATION ;
: STRING ;
: INTERFACE ;
: ENDPOINT ;

    
    
: R06 : GET_DESCRIPTOR \ [USB,189]
    
    SETUP.wValueHigh @
    ack-transaction    \ done reading SETUP data
    
                       \     -2 debug dup debug \ FE 01
    7 and route
	; DEVICE ; CONFIGURATION ; STRING ;
	INTERFACE ; ENDPOINT ; ; ;
    
: R07 : SET_DESCRIPTOR ;
: R08 : GET_CONFIGURATION ;
: R09 : SET_CONFIGURATION ;
: R0A : GET_INTERFACE ;
: R0B : SET_INTERFACE ;
    

\ route request first, handle types in subs
: SETUP
                           \ -1 debug
    SETUP.bmRequestType @  \ debug       \ need to set DATA0/DATA1 when type = #x21
    SETUP.bRequest @       \ dup debug   \ get request for routing
    #x0F and route
	R00 ; R01 ;     ; R03 ;
	    ; R05 ; R06 ; R07 ;
	R08 ; R09 ; R0A ; R0B ;
	

\ call with current object = request buffer    
: dispatch-PID \ PID --
    \ usb core gives only IN OUT STATUS,
    \ so use only top 2 bits of:
    
    \ 0001 OUT
    \ 1001 IN
    \ 1101 SETUP

    >>2
    route
	OUT ; ;
	IN  ; SETUP ;

\ this acknowledges that the input has been read completely, and sets
\ the ownerships of the endpoint buffers.
	
: ack-transaction
    -1 debug
    *EP0-OUT*
      64   BD.CNT !   \ reset byte count
      #x88 BD.STAT !  \ return OUT buffer to SIE
    *EP0-IN*
      #x08 BD.STAT !  \ return IN buffer to MCU

    UCON PKTDIS low   \ enable packet transfer (disabled by SIE after SETUP pkt)
    UIR TRNIF low     \ signal we're done handling the packet
    ;
    
: transaction

    USTAT @ #x7C and    \ mask out pingpong bit
    dup *BD*            \ current object = BD of last transaction
    >>3 endpoint ! \ save endpoint

    BD.STAT @ >>2 #x0F and  \ [PIC,174] read PID from BDnSTAT
                              dup debug  
    BD.ADRL *@*         \ current object = request buffer
    dispatch-PID
    

    \ 0 for 0 for 0 for next next next reset
    ;
    

\ : testusb init-usb begin service-usb rx-ready? until ;

    


\ error handlers
: device-error
: string-error
: config-error reset ;

    
\ utility functions
    


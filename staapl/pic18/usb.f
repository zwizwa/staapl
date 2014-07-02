\ USB driver
\ depends on Flash tables with descriptors:
\    device-descriptor        \   -- lo hi
\    configuration-descriptor \ n -- lo hi
\    string-descriptor        \ n -- lo hi


staapl pic18/shift
staapl pic18/route
staapl pic18/compose-macro
staapl pic18/afregs

\ staapl pic18/usb-generic-serial \ Descriptors for Linux Generic serial driver
\ staapl pic18/usb-acm

\ load usb-descr-usbserial.f 
\ load usb-descr-cdcacm.f 

staapl pic18/serial

\ --------------- DRIVER --------------

\ USB is tricky business.  This code was written with the help of a
\ packet sniffer (Total Phase Beagle USB 12) to make the process of
\ debugging/learning the USB enumeration process and the PIC USB
\ transceiver a bit smoother.  There is a lot of room for error, and
\ error messages on the Linux host side are usually not enough.

\ Errors made:
\  - Not buying a USB sniffer sooner
\  - General misunderstanding of how USB works (see below)
\  - Hardware: Omitted 3.3V VUSB buffer cap.
\  - Forgot to clear TRNIF
\  - DATA0/1 sync errors
\  - UADDR needs to change after IN phase is completed, not during SETUP phase.
\
\ In a nutshell, USB has 3 layers significant for writing PIC firmware
\ and making sense of sniffing, each built out of components of the
\ lower one.  Naming is from the perspective of the host.
\
\  - requests     (GetDeviceDescriptor, ...)
\  - transactions (SETUP, IN, OUT)
\  - packets      (SETUP, DATAx, ACK, STALL, ...)
\
\ The PIC transceiver interface operates at the level of transactions.
\ Each transaction uses a buffer, and data is either host->device
\ (SETUP,OUT) or device->host (IN).
\
\ E.g. :
\
\  GetDeviceDescriptor = SETUP txn, IN txn, OUT txn,
\  SETUP txn = SETUP pkt(h->d), DATA0 pkt(h->d), ACK pkt(d->h).
\  IN    txn = IN    pkt(h->d), DATA1 pkt(d->h), ACK pkt(h->d)
\  OUT   txn = OUT   pkt(h->d), DATA1 pkt(h->d), ACK pkt(d->h)



variable address      \ UADDR after current transaction
variable endpoint     \ Endpoint of current transaction
  
variable  desc-index
variable  desc-rem     \ Remaining length of current control transfer
2variable desc-next

variable usb-flags
macro
: usb-configured usb-flags 0 ;
forth

\ Cursors into USB buffers for >IN and OUT> words.
variable OUT1-read
variable IN1-write


\ PIC18 specific (since USB is also PIC specific): use rot<<, indirect
\ addressing using a register, and assumption that >r and r> can span
\ across procedures.

\ The 'a' pointer reg is used as "current RAM data stream", either for
\ in or output.  The 'f' pointer register is used for current,
\ input-only, "Flash data stream".
macro
: a> @a+ ;
: >a !a+ ;
: f> @f+ ;
forth
  

\ ep -- bd    
: IN    rot<< 1 + ;
: OUT   rot<< ;

\ Toggle mask for stat+ : bit 7 for and, bit 6 for xor.    
: DATA0  #x00 ; \ 0 and 0 xor
: DATA+  #xC0 ; \ 1 and 1 xor
: DATA1  #x40 ; \ 0 and 1 xor

macro

\ These are just needed once, so use macros.
  
: bdaddr \ n -- lo hi
    rot<< rot<< 4 ; 
: bd>a \ bd --
    bdaddr ah ! al ! ;  

: stat+ \ togglemask ustat -- ustat+  \ Update status register     
    over rot>> and xor  \ Apply togglemask to DATAx bit
    #x40 and            \ Keep only DATAx, rest was filled by USB
    #x88 or ;           \ UOWN set to USB, DTSEN=1, KEN=0, INCDIS=0, BSTALL=0

: a:bd-update \ n x --  | a:BD
    a>       \ STAT @
    stat+ >r
    !a-      \ CNT !
    r> !a ;  \ r> STAT !
forth
    
\ Prepare buffer descriptor to send to USB transceiver with updated
\ DATAx and buffer size.

: >usb \ n bd x --
    a>r
      >r bd>a r>
      a:bd-update
    r>a ;

\ -- \ ready to receive first packet = DATA0    
: OUT0-first 64 0 OUT/DATA0 ;  
: OUT1-first 64 1 OUT/DATA0 ;

\ -- \ ready to receive next packet = DATAx toggles    
: OUT1-next 64 1 OUT/DATA+ ;   
    
\ n ep --    
: OUT/DATA0 OUT DATA0 >usb ;  \ it seems DATAx is ignored for OUT?
: OUT/DATA+ OUT DATA+ >usb ;    
: IN/DATA0  IN  DATA0 >usb ;
: IN/DATA1  IN  DATA1 >usb ;    
: IN/DATA+  IN  DATA+ >usb ;    

    

  
: init-usb

    init-usb-user
    
    0 UCON !
    #xFF for next

    \ Clear event flags for user space.
    0 usb-flags !
    
    0 UIE !   \ mask all USB interrupts
    0 UIR !   \ clear all interrupt flags
    
    #x14 UCFG ! \ int pullup + transceiver, full speed, no pingpong
    #x08 UCON ! \ USBEN high: needs to be done last [PIC,164]
 
    begin UCON SE0 high? until \ wait for single ended zero on bus
    ;


: service-usb
    \ [PIC,178]
    UIR UERRIF  high? if uerror      ; then
    UIR SOFIF   high? if soframe     ; then  \ 1kHz bus heartbeat
    UIR IDLEIF  high? if idle        ; then
    UIR ACTVIF  high? if activity    ; then
    UIR STALLIF high? if stall       ; then
    UIR URSTIF  high? if usbreset    ; then
    UIR TRNIF   high? if transaction ; then
;


\ Packet layer
: uerror   0 UEIR ! ;                           \ usb error (UERRIF = OR of UEIR)
: soframe  UIR SOFIF low ;                      \ START-OF-FRAME token received
: idle     UIR IDLEIF low UCON SUSPND high ;    \ idle condition detected
: activity UIR ACTVIF low UCON SUSPND low ;     \ bus activity detected
: stall    UIR STALLIF low ;                    \ STALL handshake sent


macro
\ Implemented as macros so we can use the fast LFSR instruction.  

\ These will load the address of the endpoint register window into the
\ a register.

: buf-page 5 ;    \ second bank of dual-port USB ram (4-7)

: _buf-OUT0 #x500 ; \ 64 bytes
: _buf-IN0  #x540 ; \ 64 bytes
: _buf-OUT1 #x580 ; \ 64 bytes
: _buf-IN1  #x5C0 ; \ 64 bytes
: _buf-OUT2 #x600 ; \ 64 bytes
: _buf-IN2  #x640 ; \ 64 bytes

    
: bd-page  4 ;
    
\ It's convenient to use a current object referenced by the "a"
\ register in the code below.
  
\ prefix "a!" indicates a is changed
\ prefix "a:" indicates a is referenced
  
\ For current buffer objects, the data can then be streamed
\ into/out-of the memory using >a or a>.
    
\ Load buffer address into `a'.
: a!bd    bd-page a!! ;    
    
: a!OUT0  _buf-OUT0 2 lfsr ;
: a!IN0   _buf-IN0  2 lfsr ;
: a!OUT1  _buf-OUT1 2 lfsr ;
: a!IN1   _buf-IN1  2 lfsr ;

forth


\ Use a default map with 64 byte buffers for all endpoints.
\ EP0  500 540
\ EP1  580 5C0
\ EP2  600 640
\ EP3  680 6C0    

\ Note that code to do the address manip is a pita on a 8bit machine.
\ To simplify, factor it as "blocks".  EP0 -> 0,1  EP1 -> 2,3 etc..

: buf-addr \ n -- lo hi | address of buffer
    dup >r
    3 and rot>> rot>>
    r>
    >> >> buf-page + ;

: BD-init>a \ ep 0/1 -- | a:BD
    >r
    #x08    >a    
    64      >a
    << r> +  buf-addr >r >a r> >a ;
    
: EP-BD-init \ ep --
    a>r
    dup << << << al !  \ 8 bytes per ep
    bd-page      ah !

    dup 0 BD-init>a
        1 BD-init>a
    r>a ;
    


    
\ n -- \ Init RAM from flash.    
: f>a for f> >a next ;
    
  
\ valid usb reset occured
: usbreset
    UIR URSTIF low

    4 for UIR TRNIF low next        \ clear out the USTAT FIFO
    UEP0 #x0F a!! 16 for 0 >a next \ clear EP control regs

    \ Initialize buffer descriptors.
    0 EP-BD-init

    \ EP0 OUT BD ready for SETUP transactions.
    OUT0-first

    0 UADDR !              \ set USB address to 0
    0 UIR !                \ clear all usb interrupt flags    
    #x16 UEP0 !            \ control: in, out, setup + handshake enabled + stall off
    #xff UEIE !            \ enable all error interrupts

    0 address !
    ;


  
  

macro
: STAT>PID >>2 #x0F and ; \ convert STAT to PID.  
: a!BD/USTAT
    USTAT @ #x7C and      \ mask out bits to get BD register offset
    dup >>3 endpoint !    \ save endpoint
    4 a!! ;               \ current object = BD of last transaction
forth

\ Follow pointer.  Note this needs to load on stack and then update
\ `a' reg.  The full sequence is 8 instructions so let's make it a
\ word, not a macro.
: a!a  a> a> a!! ; 

    
: transaction

    a!BD/USTAT       \ current object = BD of last transaction in USTAT
      a> STAT>PID    \ get packet ID from BDxSTAT register
      a> drop        \ ignore size
    a!a              \ follow pointer to current buffer

    \ usb core gives only IN OUT STATUS,
    \ so use only top 2 bits to route:
    
    \ 0001 OUT    pos0
    \ 1001 IN     pos2
    \ 1101 SETUP  pos3

    [ >>2 route
        transaction.OUT   . .
        transaction.IN    .
        transaction.SETUP ] i/c

    UIR TRNIF low    \ ready for next transaction
    UCON PKTDIS low  \ clear PKTDIS flag (automatically set for SETUP packet)
        
        ;

\ These have current object set to packet buffer.        
: transaction.OUT   \ -- : a->packet
    endpoint @ 1 and route
        transaction.OUT0 .
        transaction.OUT1 ;

: transaction.OUT0 ;  \ not used


: transaction.OUT1 ;  \ NOP: task polls UOWN

\ UADDR follows address after transaction is finished.    
: transaction.IN    \ -- : a->packet
    endpoint @ 1 and route
        transaction.IN0 .
        transaction.IN1 ;
        
: transaction.IN0   \ -- : a->packet
    \ EP0: Notification of end of IN0 transaction.

    \ This event is used to wait for the end of the SET_ADDRESS
    \ transaction.  We just continuously update which seems to be OK.
    address @ UADDR !

    \ If there is any more data to pass to host in response to a
    \ control request, send it here.
    desc-rem @ 0= not if
        cont-desc setup-reply-next
    then ;

    
: transaction.IN1 ;  \ NOP: task polls UOWN

: transaction.SETUP \ -- : a->packet
    a>  \ bmRequestType
    swap-nibble >> 3 and \ type
    route
        transaction.SETUP_STANDARD .
        transaction.SETUP_CLASS    .
        . \ vendor
        ; \ reserved

: transaction.SETUP_STANDARD
    a>       \ bRequest
    
    #x0F and route
        . \ GET_STATUS        . \  0
        . \ CLEAR_FEATURE     . \  1
        .                       \  2
        . \ SET_FEATURE       . \  3
        .                       \  4
        SET_ADDRESS           . \  5
        GET_DESCRIPTOR        . \  6
        . \ SET_DESCRIPTOR    . \  7
        . \ GET_CONFIGURATION . \  8
        SET_CONFIGURATION     . \  9
        . \ GET_INTERFACE     . \ 10
        . \ SET_INTERFACE     . \ 11
        . . . ;                 \ 12-15

\ FIXME: Implement when necessary.  Not called during enumeration.
\ : GET_STATUS                ;
\ : CLEAR_FEATURE             ;
\ : SET_FEATURE               ;
\ : SET_DESCRIPTOR            ;
\ : GET_CONFIGURATION         ;
\ : GET_INTERFACE             ;
\ : SET_INTERFACE             ;


: SET_ADDRESS
    a> address !   \ store device addres
    0 setup-reply ;

: SET_CONFIGURATION
    
    \ Enable endpoint 1:
    1 EP-BD-init

    
    #x1E UEP1 !  \ IN, OUT, no SETUP, handshake, no stall

    \ EP2 is the ACM interrupt IN.  Not used.
    
    \ Enable endpoint 2:
    \ 2 EP-BD-init
    \ #x1E UEP2 !  \ IN, OUT, no SETUP, handshake, no stall
    
    0 setup-reply

    \ FIXME: should this reset pointers as before?
    \ IN1-init
    \ OUT1-init
    
    #x48 IN1/STAT bd!   \ Init to DATA1 so next transactions are DATA0,1,0,1,...
    OUT1-first          \ Prepare receive on OUT1
    usb-configured high \ Notify userspace
    ; 


: GET_DESCRIPTOR
    a> desc-index !  \ descriptor index
    a>               \ descriptor type
    a> drop
    a> drop
    a> desc-rem !    \ nb bytes to return
                     \ FIXME: high byte is ignored - only up to 255 byte descr.

    \ Numbers come from p187 USB1.1 ref.
    7 and route  \ only 3 bits?
                      . \ 0
        DEVICE        . \ 1
        CONFIGURATION . \ 2
        STRING        . \ 3
        . . . ;       . \ 4-7
: DEVICE        device-descriptor                     send-desc ;
: CONFIGURATION desc-index @ configuration-descriptor send-desc ;
: STRING        desc-index @ string-descriptor        send-desc ;

\ INTERFACE and ENDPOINT descriptors cannot be accessed directly: they
\ are concatenated to the CONFIGURATIOn descriptor.


\ This is required to handle (ignore) the ACD class request on
\ interface 0 (req=#x22).
: transaction.SETUP_CLASS
    0 setup-reply ;

    
\ Reply to a SETUP transaction.  The EP0/IN buffer contains the answer
\ data to a GET_ request.  n is the data size or 0 in case of a short
\ packet i.e. acknowledgment of a SET_ request.
: setup-reply \ n --
    OUT0-first   \ prepare EP0 OUT for reception of next SETUP request
    0 IN/DATA1 ; \ return packet in EP0/IN is DATA1
: setup-reply-next
    0 IN/DATA+ ;

\ Descriptor data is stored in Flash, prefixed a single byte
\ containing total length.  ( e.g. for configuration descriptor,
\ bLength is not the size of the full transfer as it includes
\ interface and endpoint desc. )

macro
: _@! | src dst |
    src     dst     @!
    src 1 + dst 1 + @! ;
: desc-next>f desc-next fl _@! ;    
forth
: f>desc-next fl desc-next _@! ;
    
: send-desc \ lo hi --
    f!!
    f>               \ Descriptor length on stored on Flash
    desc-rem @ min   \ Don't send more than requested : FIXME: 8 bit only!
    desc-rem !
    f>desc-next
    cont-desc
    setup-reply ;

: cont-desc \ -- n
    desc-next>f
    desc-rem @
    64 min           \ next packet size
    dup desc-rem -!
    dup a!IN0 f>a
    f>desc-next ;





\ --------------- USER --------------

\ To avoid delays, double buffering should be used.  However, delays
\ should be less than 1 ms, until the next IN token arrives.
\
\ - If EP1/IN BD is owned by USB, we have to busy-wait until
\   transaction completes.
\ - If EP/IN BD is owned by USB we can incrementally add bytes and
\   keep counter in BD.
\ - Needs disambiguation for un-initialized BD count when TX finishes.
\   Maybe IN transaction handler should clear the respective IN size?


    

\ When filling up the buffer, CNT has AL.  Strip off the bits when
\ sending it out.  We can just use the >a and a> words to access the
\ buffer.

\ Since the location of the buffer is known, these are implemented as
\ macros to make the other code a bit more readable, and to have a
\ more efficient implementation.  Indirect addressing is inefficient
\ since we're already using all 3 pointer registers.

macro
: IN>BD    OUT>BD 4 + ;          \ EP -- BD
: OUT>BD   8 * ;                 \ EP -- BD
: CNT      1 + ;                 \ BD -- BD.CNT
: IN1/STAT 1 IN>BD ;             \ -- BD.IN1.STAT    
: IN1/CNT  1 IN>BD CNT ;         \ -- BD.IN1.CNT
: OUT1/CNT 1 OUT>BD CNT ;        \ -- BD.OUT1.CNT
: bd@      >m bd-page b! m> @b ; \ addr -- value (fetch in BD page)
: bd!      >m bd-page b! m> !b ; \ value addr -- (store in BD page)

: BUFSIZE  64 ;
: OUT>BUF  BUFSIZE * 2 * ;
: IN>BUF   OUT>BUF BUFSIZE + ;
    
forth

macro
: =? - nfdrop z? ; \ a b -- ?
forth

\ ** ISR **
 
: init-usb-isr
    init-usb
    RCON IPEN high    \ enable priority levels
    INTCON GIEH high  \ high priority ie
    INTCON GIEL high  \ low priority ie
    IPR2 USBIP low    \ usb low priority
    #x7F UIE !        \ enable all USB interrupts (ORed to produce USBIF)
    PIE2 USBIE high   \ enable USBIF interrupt
    \ Wait for device to be configured
    begin usb-configured high? until ;

: lo-isr
    \ Save/restore STATUS is special because DROP uses MOVF, which
    \ modifies flags (see boot.ss).  The rest of the machine model can
    \ run from interrupt as long as INDF0/INDF1 point to proper stack
    \ space.
    STATUS@
    
    PIR2 USBIF high? if
        PIR2 USBIF low

        \ Save/restore RAM/Flash byte pointers since service-usb clobbers.
        a>r f>r
        service-usb
        r>f r>a
    then

    \ Restore status (see boot.ss)
    STATUS!
    0 retfie ;

\ USB is fixed at low priority interrupt.    
' lo-isr init-isr-lo

 

\ ** WRITING TO IN1 **  
  
\ The IN1 buffer can be in one of two states:
\ - BD.STAT.UOWN=1 Owned by USB: a transaction is ongoing and we're not allowed to write
\ - BD.STAT.UOWN=0 Owned by UC, we can write to IN1 buffer + descriptor

macro
: a!IN1.STAT  12 4 a!! ;
: a!OUT1.STAT  8 4 a!! ;
: a:STAT.UOWN INDF2 7 ;    
forth
: a:wait-UOWN begin a:STAT.UOWN low? until ;  


load usb-user.f
: OUT1>     1 OUT> ;
: >IN1      1 >IN ;
: IN1-flush 1 IN-flush ;

\ load usb-generic.f
    
    
    

\ ** TEST **

    
\ : >debug async.>tx ; \ 10 async.>tx ;    
\ : init-debug 230400 48000000 init-serial ;
    
  
\ : test-loopback
\     begin OUT1> >IN1 again
    
\ : testi
\     \ init-debug
\     init-usb-isr
\     test-loopback ;

\ : testx
\     \ init-debug
\     init-usb-isr
\     0 begin OUT1> drop dup >IN1 1 + again ;

\ : test
\     init-usb
\     begin service-usb again

\ : testc
\     init-usb
\     begin service-usb usb-configured high? until ;
        
\ : testl
\     testc
\     IN1-demo
\     begin service-usb again
    
\ Service for a bit, then soft-reset PIC.
\ : test0
\     init-usb
\     30 for 255 for 255 for service-usb next next next
\     warm ;


    
\ : IN1-demo
\     \ Send some data to IN1
\     a!IN1 32 63 for dup >a 1 + next drop
\     10 >a \ LF
\     64 1 IN/DATA+ ;
    

\ load debug.f

    
\ staapl pic18/serial
\ macro
\ : fosc 48 MHz ;
\ : baud 230400 ;
\ : >debug async.>tx ;
\ : init-debug init-comm ;    
\ forth



\ Compilers for USB descriptors defined as raw byte tables
\ (e.g. gathered using 'scheme from a Scheme file)
    
staapl pic18/compose-macro

macro
: descriptor-compiler \ list -- table-compiler
    [ table-> ] swap >macro compose-macro
    [ ' , for-list ]        compose-macro ;

\ These compile to Flash a single descriptor: -- lo hi    
: usb-descriptor
    descriptor-compiler compile ;    
\ or an array of descriptors: n -- lo hi
: usb-descriptors
    \ FIXME: no bounds check
    >m ' route compile m>
        [ descriptor-compiler i/c* . ]
        for-list ;

\ see dip40kit.fm for example
forth

staapl pic18/shift
staapl pic18/route
staapl pic18/usb-generic-serial \ Descriptors for Linux Generic serial driver

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
\ Each transaction uses a buffer, and data is either read (SETUP,OUT)
\ or written (IN).
\
\ E.g. :
\
\  GetDeviceDescriptor = SETUP txn, IN txn, OUT txn,
\  SETUP txn = SETUP pkt(h->d), DATA0 pkt(h->d), ACK pkt(d->h).
\  IN    txn = IN    pkt(h->d), DATA1 pkt(d->h), ACK pkt(h->d)
\  OUT   txn = OUT   pkt(h->d), DATA1 pkt(h->d), ACK pkt(d->h)



variable address    \ UADDR after current transaction
variable endpoint   \ Endpoint of current transaction
variable index
variable length

variable usb-flags
macro
: usb-configured usb-flags 0 ;
forth

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

: a/bd-update \ n x --  | a:BD
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
      a/bd-update
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
: buf-OUT0 #x00 ; \ 64 bytes
: buf-IN0  #x40 ; \ 64 bytes
: buf-OUT1 #x80 ; \ 64 bytes
: buf-IN1  #xC0 ; \ 64 bytes

: bd-page  4 ;
    
forth


  

\ Words that end in ':' set the current object pointer (the `a'
\ register) to a particular address.  The data can then be streamed
\ into/out-of the memory using !+ or @+.
    
\ Load buffer address into `a'.
: a!buf   buf-page a!! ;
: a!bd    4 a!! ;    
    
: a!OUT0  buf-OUT0 a!buf ;
: a!IN0   buf-IN0  a!buf ;
: a!OUT1  buf-OUT1 a!buf ;
: a!IN1   buf-IN1  a!buf ;

forth


: EP0-init
    table->
      \ BD0 : EP0 OUT
      #x08     ,  \ BD0STAT: set UOWN, MCU can write, DTSEN=1
      64       ,  \ BD0CNT
      buf-OUT0 ,  \ BD0ADRL
      buf-page ,  \ BD0ADRH

      \ BD1 : EP0 IN
      #x08     ,  \ BD1STAT: clear UOWN, MCU can write, DTSEN=1
      64       ,  \ BD1CNT
      buf-IN0  ,  \ BD1ADRL
      buf-page ,  \ BD1ADRH

: EP1-init
    table->
      \ BD2 : EP1 OUT
      #x08     ,  \ BD2STAT: set UOWN, MCU can write, DTSEN=1
      64       ,  \ BD2CNT
      buf-OUT1 ,  \ BD2ADRL
      buf-page ,  \ BD2ADRH

      \ BD3 : EP1 IN
      #x08     ,  \ BD3STAT: clear UOWN, MCU can write, DTSEN=1
      64       ,  \ BD3CNT
      buf-IN1  ,  \ BD3ADRL
      buf-page ,  \ BD3ADRH


\ n -- \ Init RAM from flash.    
: f>a for f> >a next ;
    
  
\ valid usb reset occured
: usbreset
    UIR URSTIF low

    4 for UIR TRNIF low next        \ clear out the USTAT FIFO
    UEP0 #x0F a!! 16 for 0 >a next \ clear EP control regs

    \ Initialize buffer descriptors.
    0 4 a!! EP0-init f!! 8 f>a

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
    \ EP0: For control pipe, this is only used to wait for the end of
    \ the SET_ADDRESS transaction.  We just continuously update which
    \ seems to be OK.
    address @ UADDR ! ;

    
: transaction.IN1   \ NOP: task polls UOWN

: transaction.SETUP \ -- : a->packet
    a>  drop \ bmRequestType
    a>       \ bRequest
    
    #x0F and route
        GET_STATUS        . \  0
        CLEAR_FEATURE     . \  1
                          . \  2
        SET_FEATURE       . \  3
                          . \  4
        SET_ADDRESS       . \  5
        GET_DESCRIPTOR    . \  6
        SET_DESCRIPTOR    . \  7
        GET_CONFIGURATION . \  8
        SET_CONFIGURATION . \  9
        GET_INTERFACE     . \ 10
        SET_INTERFACE     ; \ 11



: SET_ADDRESS
    a> address !   \ store device addres
    0 setup-reply ;

: SET_CONFIGURATION
    
    \ Enable endpoint 1:
    8 4 a!! EP1-init f!! 8 f>a \ Init EP1 buffer descriptors
    #x1E UEP1 !  \ IN, OUT, no SETUP, handshake, no stall
    0 setup-reply
    
    IN1-init            \ BD.CNT=0
    #x48 IN1/STAT bd!   \ Init to DATA1 so next transactions are DATA0,1,0,1,...
    OUT1-first          \ Prepare receive on OUT1
    usb-configured high \ Notify userspace
    ; 


: GET_DESCRIPTOR
    a> index !  \ descriptor index
    a>          \ descriptor type
    a> drop
    a> drop
    a> length ! \ nb bytes to return

    \ Numbers come from p187 USB1.1 ref.
    7 and route  \ only 3 bits?
                      . \ 0
        DEVICE        . \ 1
        CONFIGURATION . \ 2
        STRING        . \ 3
        INTERFACE     . \ 4
        ENDPOINT      . \ 5
                      . \ 6  \ Linux 2.6.33.7-rt29 sends this.  WTF?
                      ; \ 7
: DEVICE        device-descriptor                send-desc ;
: CONFIGURATION index @ configuration-descriptor send-desc ;
: STRING        index @ string-descriptor        send-desc ;
: INTERFACE             ;
: ENDPOINT              ;   

\ FIXME: Implement when necessary.  Not called during enumeration.    
: GET_STATUS                ;
: CLEAR_FEATURE             ;
: SET_FEATURE               ;
: SET_DESCRIPTOR            ;
: GET_CONFIGURATION         ;
: GET_INTERFACE             ;
: SET_INTERFACE             ;


    
    

    
\ Reply to a SETUP transaction.  The EP0/IN buffer contains the answer
\ data to a GET_ request.  n is the data size or 0 in case of a short
\ packet i.e. acknowledgment of a SET_ request.
: setup-reply \ n --
    OUT0-first   \ make room for next SETUP request on EP0/OUT
    0 IN/DATA1 ; \ return packet in EP0/IN is DATA1

\ The device descriptor data is stored in Flash, generated from high
\ level config.  See e.g. usb-cdc.ss
: copy-desc \ lo hi -- size \ Copy descriptor from Flash to USB RAM.
    a!IN0 f!! f> dup for f> >a next ;
: send-desc \ lo hi --
    copy-desc \ n 
    length @ min \ min \ Don't send more than requested
    setup-reply ;





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



    
: IN1-demo
    \ Send some data to IN1
    a!IN1 32 63 for dup >a 1 + next drop
    10 >a \ LF
    64 1 IN/DATA+ ;
    

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
    PIR2 USBIP low    \ usb low priority
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

' lo-isr init-isr-lo

 

\ ** WRITING TO IN1 **  
  
\ The IN1 buffer can be in one of two states:
\ - BD.STAT.UOWN=1 Owned by USB: a transaction is ongoing and we're not allowed to write
\ - BD.STAT.UOWN=0 Owned by UC, we can write to IN1 buffer + descriptor

macro
: a/IN1.STAT  12 4 a!! ;
: a/OUT1.STAT  8 4 a!! ;
: a/STAT.UOWN INDF2 7 ;    
forth
: a/wait-UOWN begin a/STAT.UOWN low? until ;  

\ Two interfaces are provided:
\  high level:  >IN1 IN1-flush
\  low level:   a/IN1-begin a/IN1-write a/IN1-end

\ The" a/IN1-begin" and "a/IN1-end" words define a context in which
\ the "a/IN1-write" word is valid.  These words use (clobber) the "a"
\ register to make transferring multiple bytes more efficient.  The
\ ">IN1" word is a highlevel byte-per-byte write function.  Note that
\ as long as the data fits in the buffer, it is possible to use just
\ ">a" instead of "a/IN1-write", which omits the buffer size check.

: a/IN1-begin
    a/IN1-wait    \ wait until IN1's contents is transferred to host.
    buf-IN1 buf-page a!!
    IN1-write @ al +! ;

: a/IN1-wait
    a/IN1.STAT a/wait-UOWN ;

: a/IN1-write >a
    al @ buf-IN1 -
    BUFSIZE =? if a/IN1-transaction then ;

: a/IN1-transaction
    a/IN1-end     \ end IN1 buffer write session
    IN1-flush     \ transfer IN1 buffer to USB
    a/IN1-begin ; \ start a new IN1 buffer write session
    
: a/IN1-end
    al @ buf-IN1 - IN1-write ! ;

\ When IN1 buffer is filled by UC, "IN1-flush" will transfer ownership
\ to the USB peripheral.  It will perform an IN transaction with the
\ host, and when it is done will get notified through the
\ transaction.IN1 word in the ISR, at which point the buffer ownership
\ is returned to the UC.  That word will call "IN1-init" to reset the
\ write count.
    
: IN1-flush   IN1-write @  IN1-init 1 IN/DATA+ ;
: IN1-init    0 IN1-write ! ;


\ Just one byte.  This flushes only when necessary.  It is allowed to
\ call "IN1-flush" after ">IN1" to force a transaction.  The next
\ ">IN1" will busy-wait until the previous transaction is done.
: >IN1 a>r a/IN1-begin a/IN1-write a/IN1-end r>a ; \ byte --


\ ** READING FROM OUT1 **  
 
: a/OUT1-begin
    a/OUT1-wait  \ wait until OUT1 has data from host
    buf-OUT1 buf-page a!!
    OUT1-read @ al +! ;
    
: a/OUT1-wait
    a/OUT1.STAT a/wait-UOWN ;

: a/OUT1-read
    al @ OUT1/CNT bd@ -
    buf-OUT1 =? if a/OUT1-transaction then
    a> ;
 
: a/OUT1-transaction
    \ a/OUT-end    \ not necessary (kept here for symmetry with a/IN1-transaction)
    OUT1-fill      \ transfer OUT1 ownership to USB to collect new data
    a/OUT1-begin ; \ start new transaction to OUT1 buffer

: a/OUT1-end
    al @ buf-OUT1 - OUT1-read ! ;

\ When OUT1 buffer is filled by USB, the UC is notified through the
\ transaction.OUT1 word in ISR.  That word will call "OUT1-init" to
\ reset the read count.

macro
: OUT1-fill  0 OUT1-read ! OUT1-next  ;
forth

    
\ Just one byte.  This clears frees the OUT1 buffer when necessary to
\ request more data from host.
\ NOTE: It might be good to flush the IN1 buffer to host before
\ starting a busy wait on OUT1 from host.  
: OUT1> a>r a/OUT1-begin a/OUT1-read a/OUT1-end r>a ; \ -- byte





\ ** TEST **

    
\ : >debug async.>tx ; \ 10 async.>tx ;    
\ : init-debug 230400 48000000 init-serial ;
    
  
: test-loopback
    begin OUT1> >IN1 again
    
: testi
    \ init-debug
    init-usb-isr
    test-loopback ;

: testx
    \ init-debug
    init-usb-isr
    0 begin OUT1> drop dup >IN1 1 + again ;

    
: test
    init-usb
    begin service-usb again

: testc
    init-usb
    begin service-usb usb-configured high? until ;
        
: testl
    testc
    IN1-demo
    begin service-usb again
    
\ Service for a bit, then soft-reset PIC.
: test0
    init-usb
    30 for 255 for 255 for service-usb next next next
    warm ;
    

load debug.f

    
\ staapl pic18/serial
\ macro
\ : fosc 48 MHz ;
\ : baud 230400 ;
\ : >debug async.>tx ;
\ : init-debug init-comm ;    
\ forth



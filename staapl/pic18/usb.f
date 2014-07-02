\ USB driver
\ depends on Flash tables with descriptors:
\    device-descriptor        \   -- lo hi
\    configuration-descriptor \ n -- lo hi
\    string-descriptor        \ n -- lo hi


staapl pic18/shift
staapl pic18/route
staapl pic18/compose-macro
staapl pic18/afregs
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


\ --- LOWLEVEL

\ It's convenient to use a current object referenced by the "a"
\ register in the code below.  For current buffer objects, the data
\ can then be streamed into/out-of the memory using >a or a>.
  
\ prefix "a!" indicates a is changed and valid upon return
\ prefix "a:" indicates a is referenced in a word

macro
\ dual port USB ram:
\ 400-... buffer descriptors, 4 bytes per buffer
\ 4F0-4FF buffer current user read/write pointers (can go in normal ram)
\ 500-7FF 64 byte buffers for 6 endpoints
: bd-page      4 ;
: buf-page     5 ;
: a!iptr-array #x4F0 2 lfsr ;
: a!IN0        #x540 2 lfsr ;
    
forth
  
: buf-addr-lo 3 and rot>> rot>> ; \ n -- lo
: buf-addr-hi >> >> buf-page +  ; \ n -- hi
: buf-addr \ n -- lo hi | address of buffer
    dup >r buf-addr-lo
        r> buf-addr-hi ;

variable address      \ UADDR after current transaction
variable endpoint     \ Endpoint of current transaction
  
variable  desc-index
variable  desc-rem     \ Remaining length of current control transfer
2variable desc-next

variable usb-flags
macro
: usb-configured usb-flags 0 ;
forth


\ Lowlevel.  Not used in user code, but convenient to use the buf
\ variable to access buffer descriptors in boot / isr code.

\ Used by kernel during enumeration.  After that it's user owned only.    
variable buf

: OUT! << buf ! ;
: IN!  << 1 + buf ! ;    

: a!UEP
    UEP0 2 lfsr ep al +! ;
    
: bufdes-rst
    a!bufdes
    #x08 >a
    64   >a
    buf @ buf-addr-lo >a ;
    buf @ buf-addr-hi >a ;
    
: EP-BD-init \ ep --
    dup OUT! bufdes-rst
        IN!  bufdes-rst ;
    
  

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
: =? - nfdrop z? ; \ a b -- ?
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
\ All ignored.  OUT0 doesn't need it and user code will poll UOWN.
: transaction.OUT ;  \ -- : a->packet


\ UADDR follows address after transaction is finished.
\ Ignored for all but IN0.  User polls UOWN.    
: transaction.IN    \ -- : a->packet
    endpoint @ 1 min route
        transaction.IN0 . ;
        
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

    \ Enable user endpoints starting at EP1
    1 OUT!
    total-EP 1 - for
        \ init OUTx
        bufdes-rst iptr-rst
        64 ep OUT/DATA0  \ prepare to receive on OUT0
        1 buf +!
        
        \ init INx
        bufdes-rst iptr-rst
        a!bufdes #x48 >a \ set DATA1. next transactions are DATA0,1,0,...

        \ EPx
        a!UEP #x1E >a  \ UEPx: IN, OUT, no SETUP, handshake, no stall
        1 buf +!
    next
    
    0 setup-reply
    usb-configured high ;  \ Userspace waits for this


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

\ To avoid delays, double buffering could be used.  However, delays
\ should be less than 1 ms, until the next IN token arrives.
\

\ Main routines:
\   OUT>     \ ep -- val  |  read from EP OUT
\   >IN      \ ep val --  |  write to  EP IN
\   IN-flush \ ep --      |  flush write on EP IN

\ Note that OUT/IN names are from the perspective of the host.


\ Implementation: use idiomatic "current object" Forth approach.
\ This uses busy polling (BD:UOWN)
\ For ISR access, hook into transaction.OUTx/INx
\ Buffer can be in one of two states:
\ - BD.STAT.UOWN=1 Owned by USB: a transaction is ongoing and we're not allowed to write
\ - BD.STAT.UOWN=0 Owned by UC, we can read or write buffer + descriptor



\ Generic access needs:
\  - current buffer          #x500, #x540, ...
\  - index variables         #x4F0, #x4F1, ...
\  - USB buffer descriptors  #x400, #x404, ...  (OUT0, IN0, OUT1, IN1, ...)


   

\ 16-bit pointer chasing on the stack is a bit of a pain in an 8 bit
\ Forth, so use the a register.  Use address loading words a!xxx in
\ conjunction with >a a> etc..

: ep       buf @ >> ;
: a!bufdes buf @ << << bd-page a!! ;       \ buffer descriptor
: a!buf    buf @ buf-addr a!! ;            \ buffer start
: a!iptr   a!iptr-array buf @ al +! ;      \ index register address

: idx      a!iptr a> ;                     \ -- i | just get index
: idx+     a!iptr a>r a> dup 1 +  r>a >a ; \ -- i | get index, postincrement variable
: a!box+   idx+ #x3F and a!buf al +! ;     \ a points to "box", index is incremented

: iptr-rst a!iptr 0 >a ;

: bd-len   a!bufdes a> drop a> ;


: bd-wait  a!bufdes begin INDF2 7 low? until ; \ poll UOWN until we own the bd
    

\ pump: do IN / OUT transaction if necessary    
: pump-OUT
    idx bd-len =? not if ; then
    64 ep OUT/DATA+
    iptr-rst bd-wait ;

: pump-IN
    idx #x40 =? not if ; then
: force-pump-IN
    idx ep IN/DATA+
    iptr-rst bd-wait ;
    

: OUT> \ ep -- val
    OUT! a>r
      bd-wait     \ make sure buffer is ready
      pump-OUT    \ if fully read, ack buffer and wait for more data from host
      a!box+ a>   \ read, advancing index
    r>a ;

: >IN  \ val ep --
    IN! a>r
      bd-wait     \ make sure buffer is ready
      pump-IN     \ if buffer is full, send it to host and wait until we can write more
      a!box+ >a   \ write, advancing index
    r>a ;

: IN-flush \ ep --
    IN! force-pump-IN ;
    


\ debug
\ : pa al @ ah @ ` _px host ;
\ : pbuf a!buf 64 for a> ` px host next ;

  

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


\ Compilers for USB descriptors defined as raw byte tables
\ (e.g. gathered using 'scheme from a Scheme file)
    

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




\ --- INTERPRETER ---

staapl pic18/stdin
staapl pic18/stdout

\ NOTE: This protocol will not be kept backwards compatible.
\ The main goal is to be simple and support all targets.

\ Protocol: half-duplex ping/pong transaction, accessible as a byte
\ stream with explicit sync points to allow for data-ready or
\ packet buffering.
\
\ Currently there are 3 transport layers:


\ - serial async: doesn't need sync points
\ - USB serial / ACM: needs flush at end
\ - PK2 / ICSP synchronous serial protocol with
\   handshake for bus direction arbitration.
\
\ Note that the ICSP variant has size restrictions for the size of the
\ packets.
\
\ This protocol can embed arbitrary binary messages sent to different
\ addresses (!= 0 == us).  If we are the destination, data will be
\ interpreted using the `interpret-cmd' table below.  The data
\ protocol is structured in such a way that buffering is not
\ necessary, apart from loading parameters on the data stack.
\
\
\ RX-SYNC RX-DATA                         TX-SYNC TX-DATA                    TX-END
\ ---------------------------------------------------------------------------------
\   |     <addr> <size> <cmd> [<arg> ...]    |    <addr> <size> [<data> ...]   |
\
\ rx-sync \ --    \ after this receive / transmit are valid.
\
\ receive \ -- b  \ destination address. 0 = us, >0 : daisy-chain forward (decrement address)
\ receive \ -- b  \ number of payload bytes, necessary for opaque forwarding, ignored for interpretation (redundant: data is self-delimiting)
\ receive \ -- b  \ interpreter command
\ [ receive ...]  \ interpreter command's arguments
\
\ tx-sync \ --
\
\ transmit \ b --  \ address
\ transmit \ b --  \ nb bytes
\ [ transmit ...]  \ payload
\
\ tx-end

\ Begin a reply to host.  Terminate with tx-end, which in turn is
\ followed interpret-packet (rx-sync) to handle the next packet.

: tx-begin-reply \ size --
    #xFF   \ reply to host
: tx-begin-addr \ size addr --
    tx-sync
    transmit
    transmit ;

\ Start a host RPC call.  Size includes RPC function tag == first
\ byte.  Finish message with tx-end-rpc.
    
: tx-begin-rpc \ size --
    #xFE tx-begin-addr ;


\ Acknowledgmenet is a 0-size reply.  For variable size payloads see use of
\ tx-begin-reply / tx-end below.
: _._ \ unused slot
: ack 0 tx-begin-reply tx-end ;
    




\ The interpreter tokens implement only the bare minimum: memory
\ transfer for stack/RAM/Flash and calling native code.

\ Token 0 is nop, so a stream of zeros can be used as soft interrupt

\ All tokens send a reply packed for synchronization except for `jsr'
\ which is used for jumping to interpreter extensions which will send
\ reply.

\ token --
: interpret-cmd
    #x0F and route
     ack  . npush . npop . jsr  .
     lda  . ldf   . _._  . intr .
     n@a+ . n@f+  . n!a+ . n!f+ .
     _._  . _._   . _._  . _._  ;

: interpret-msg
    receive 0= if ; then     \ empty messages are NOP, other are commands
    receive interpret-cmd ;  \ start interpreting the byte stream (we don't need count)


\ Bytecode interpreter main loop.  The zero length message is ignored,
\ all other messages are interpreted and assumed to be of correct
\ length.  ( This is debug: target doesn't need to second-guess the host. )

macro
: 0=  #xFF + drop nc? ;        \ n -- ?
: 0?  -1 addlw 1 addlw z? ;    \ n -- n ?  : sets machine flags
forth


\ The first byte in the packet is the device address.  We are #x00,
\ any other we need to forward if we're a hub.
: interpret-packet
    \ Handle protocol preamble.  All other code needs to properly
    \ terminate transmission.
    rx-sync
    receive 1 - nc? if
        \ 0 = us
        drop interpret-msg ;
    then
        \ forward to other (only for hub nodes)
        forward-msg ;

: interpreter
    begin interpret-packet again


\ Interpreter extension.  Code called with this token should send a
\ reply message.
: intr push receive2 TOSH ! TOSL ! ;

\ Execute code and synchronize with empty reply.
: jsr intr ack ;

\ While target code is running, it is possible to send an RPC call
\ from target by replacing the `ack' reply with a reply containing an
\ RPC request followed by an invokation of the interpreter to receive
\ the RPC answer.  Essentially the target->host RPC protocol is the
\ host->target protocol turned "inside out".  See debug.f

: receive3 receive
: receive2 receive receive ;

\ Block transfer.  These are "context-free", meaning they do not need
\ to retain context on the target to complete a single transfer, which
\ is more robust.
  
: i>host \ --
    receive dup tx-begin-reply for
        i> transmit
    next tx-end ;

: host>o \ --
    receive for receive >o next ack ;
  
\ Read/write for datastack, RAM, Flash

: npop  i=d i>host ;
: n@a+  i=a i>host ;
: n@f+  i=f i>host ;

: npush o=d host>o ;
: n!a+  o=a host>o ;
: n!f+  o=f host>o ;

\ pointer initialization
: lda  receive2 a!! ack ;
: ldf  receive3 f!!! ack ;




\ Utility words.  These are ordinary Forth words that do not perform
\ any host transactions.  Host can call these using jsr.

: ferase   flash erase   ; 
: fprog    flash program ; 
    
\ Program block memory check.  See also chkblk8 in fast-chkblk.f
: chkblk 255 64 for @f+ and next ; 

\ Dump data stack pointer and a reg.
: stackptr FSR0L @ FSR0H @ ;
  
\ Interactive commands: return operator control back to the console in
\ various ways, and need 'ack' to notify the host host we're ready to
\ except new commands.

\ Note that 'cold' is a host command which performs an external reset
\ or power cycle.  'bp' is non-standard forth word: it recursively
\ calls the interpreter and thus acts as a software breakpoint.  it
\ can be resumed by 'continue' (debug.f)
    
: warm  init-warm    
: abort init-abort
: quit  init-quit
: bp    ack interpreter ;



\ No forwarding.  This is a stub that allows the target counting code
\ to work, which sends a zero length message around the chain by
\ picking the highest address 255.

macro    
: forward-msg-ignore
    receive drop  \ drop size 0 message
    tx-begin-reply
    transmit      \ address
    0 transmit
    tx-end ;
forth

\ Call this from mainloop to handle console commands.
\ Send `ack' before starting the mainloop.
macro
: poll-interpreter cmd-ready? if interpret-packet then ;  
forth
  
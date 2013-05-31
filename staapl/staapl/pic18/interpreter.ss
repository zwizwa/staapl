#lang planet zwizwa/staapl/pic18 \ -*- forth -*-
provide-all

staapl pic18/vector
staapl pic18/prom
staapl pic18/route

2variable v-rx
2variable v-tx
: receive  v-rx invoke ;
: transmit v-tx invoke ;

\ macro
\ not necessary for uart transport    
: wait-ack ;  
: rx-handshake ;
: tx-wait-ack ;
\ forth

\ --- SERIAL SPECIFIC
: tx-handshake  #xFF transmit transmit ;  \ size -- : reply packet (address + length)

: forward-msg \ id --
    transmit
    receive dup transmit
    0? if drop ; then
    for receive transmit next ;



\ Include interpreter.  Note that the `proper' way is to use units for
\ this kind of pluggability, but there is something to say for the
\ usefulness of incrementally loaded code in combination with early
\ binding.

\ This code requires: handshake, ack, transmit, receive    
load interpreter.f

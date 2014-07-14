#lang staapl/pic18 \ -*- forth -*-
provide-all

variable async-or
variable async-fe

macro

: async.rx-reset
    RCSTA CREN low
    RCSTA CREN high ;

: async.overrun?   RCSTA OERR high? ;
: async.frame?     RCSTA FERR high? ;

: async.rx-ready?  PIR1  RCIF high? ;
: async.tx-ready?  TXSTA TRMT high? ;

forth  

: async.rx>
    begin async.rx-ready? until
    async.overrun? if
        async.rx-reset
        async-or 1+!
        async.rx> ;
    then
    RCREG @
    async.frame? if
        drop
        async-fe 1+!
        async.rx> ;
    then ;

: async.>tx
    begin async.tx-ready? until TXREG ! ;


  
              

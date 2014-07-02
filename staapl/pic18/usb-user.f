\ FIXME:

\ Generic >INx  OUTx>  (or multi-byte equivalents or data transfers)
\ Implementation: use idiomatic "current object" Forth approach.

\ Generic access needs:
\  - current buffer          #x500, #x540, ...
\  - index variables         #x4F0, #x4F1, ...
\  - USB buffer descriptors  #x400, #x404, ...  (OUT0, IN0, OUT1, IN1, ...)

staapl pic18/afregs

\ Compiled macros  
: *a!! a!! ;


\ USER ACCESS: Buffered polling only.
\ For ISR access, hook into transaction.OUTx/INx
    
variable buf
: init-usb-user #xF0 4 *a!! 16 for 0 >a next ;
    

\ 16-bit pointer chasing is a bit of a pain in an 8 bit Forth, so use
\ the a register.  Separate address loading selector words a!xxx from
\ access through a register using >a a> etc..

: ep       buf @ >> ;
: a!bufdes buf @ << << bd-page *a!! ;  \ buffer descriptor
: a!buf    buf @ buf-addr *a!! ;       \ buffer start
: a!iptr   #xF0 4 *a!! buf @ al +! ;    \ index register address

: idx      a!iptr a> ;                     \ -- i | just get index
: idx+     a!iptr a>r a> dup 1 +  r>a >a ; \ -- i | get index, postincrement variable
: a!box+   idx+ #x3F and a!buf al +! ;     \ a points to "box", index is incremented

: iptr-reset a!iptr 0 >a ;

: bd-len   a!bufdes a> drop a> ;
    
: flush-OUT 64 ep OUT/DATA+  iptr-reset ;
: flush-IN  ` flush-IN  .sym iptr-reset ;
    
: pump-OUT idx bd-len =? if flush-OUT bd-wait then ;
\ : pump-IN  idx #x40 =? if flush-IN  then ;

  
: bd-wait a!bufdes a:wait-UOWN ;    \ wait until we own the bd


: OUT> \ ep -- val
    << buf ! a>r
      bd-wait
      pump-OUT
      a!box+ a>
    r>a ;

\ : >IN   << 1 +  buf !                  a>r a!box+ >a pump-IN  r>a ;
    

\ debug
: pa al @ ah @ ` _px host ;
: pbuf a!buf 64 for a> ` px host next ;

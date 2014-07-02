\ FIXME:

\ Generic >INx  OUTx>  (or multi-byte equivalents or data transfers)
\ Implementation: use idiomatic "current object" Forth approach.

\ Generic access needs:
\  - current buffer          #x500, #x540, ...
\  - index variables         #x4F0, #x4F1, ...
\  - USB buffer descriptors  #x400, #x404, ...  (OUT0, IN0, OUT1, IN1, ...)

staapl pic18/afregs



\ USER ACCESS: Buffered polling only.
\ For ISR access, hook into transaction.OUTx/INx

 

\ Buffer can be in one of two states:
\ - BD.STAT.UOWN=1 Owned by USB: a transaction is ongoing and we're not allowed to write
\ - BD.STAT.UOWN=0 Owned by UC, we can read or write buffer + descriptor



    
variable buf
    

\ 16-bit pointer chasing is a bit of a pain in an 8 bit Forth, so use
\ the a register.  Separate address loading selector words a!xxx from
\ access through a register using >a a> etc..

: ep       buf @ >> ;
: a!bufdes buf @ << << bd-page a!! ;        \ buffer descriptor
: a!buf    buf @ buf-addr a!! ;             \ buffer start
: a!iptr   index-array 2 lfsr buf @ al +! ; \ index register address

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
    

: OUT! << buf ! ;
: IN!  << 1 + buf ! ;    
    
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
    

\ Lowlevel.  Not used in user code, but convenient to use the buf
\ variable to access buffer descriptors in boot / isr code.

: a!UEP      UEP0 2 lfsr ep al +! ;    
: bufdes-rst
    a!bufdes
    #x08 >a
    64   >a
    buf @ buf-addr-lo >a ;
    buf @ buf-addr-hi >a ;
    

\ debug
\ : pa al @ ah @ ` _px host ;
\ : pbuf a!buf 64 for a> ` px host next ;

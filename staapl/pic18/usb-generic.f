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


variable buf

\ 16-bit pointer chasing is a bit of a pain in an 8 bit Forth, so use
\ the a register.  Separate address loading selector words a!xxx from
\ access through a register using >a a> etc..


: a!bd   buf @ << << bd-page *a!! ;  \ buffer descriptor
: a!buf  buf @ buf-addr *a!! ;       \ buffer start
: a!iptr 4 #xF0 a!! buf @ al +! ;    \ index register address

: idx    a!iptr a> ;                    \ -- i | just get index
: idx+   a!iptr a>r a> dup 1 + r>a >a ; \ -- i | get index, postincrement variable
: a!box+ idx+ a!buf al +! ;             \ a points to "box", index is incremented

: pump-OUT idx #x40            =? if flush-OUT then ;
: pump-IN  idx a!bd a> drop a> =? if flush-IN  then ;

: flush-OUT ;
: flush-IN ;    
  
: bd-wait a!bd a:wait-UOWN ;    \ wait until we own the bd

: OUT{ <<     buf{ ;
: IN{  << 1 + buf{ ;    
: buf{ buf ! a>r a!box+ ;
: }OUT pump-OUT r>a ;
: }IN  pump-IN  r>a ;   

: OUT>  OUT{ a> }OUT ;
: >IN    IN{ >a }IN ;
    

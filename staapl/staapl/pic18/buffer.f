\ Macros for generic 2^n byte buffers that can be specialized with
\ static knowledge, because indirect access is expensive on PIC, and
\ buffers usually need to be fast. Statically known data:
\  - base address
\  - size (specified as bitmask)
\  - state: location of read / write pointers: sequential byte variables

\ Read and write pointers are distinct to ensure proper concurrent
\ access.

\ Note that this code is optimized for speed and not code size: it's
\ up to you to instantiate some words manually if you think
\ 'buffer.read/yield' and 'buffer.read/write' give too much machine
\ code.


macro

: a! 2 lfsr ;   \ set literal (large) value in a reg  
: buffer.index | mask | @ mask and al +! ;
: p.read ;
: p.write 1 + ;    

\ ops on buffer data
    
: buffer.read  | base p mask |
    base a!
    p p.read mask buffer.index @a
    p p.read 1+! ;
    
: buffer.write | base p mask |
    base a!
    p p.write mask buffer.index !a
    p p.write 1+! ;

\ ops on pointers only. note that size takes the actual pointers, not
\ the masked ones.

: p.size | p |
    p p.write @
    p p.read @ - ;
    
: buffer.size | base p mask |
    p p.size ;

\ check if both pointers are the same    
: buffer.empty>z | base p mask |
    p p.write @
    p p.read @ xor nfdrop ;

\ check if no overflow has occured (high bits of size are 0)
: buffer.room>z | base p mask |
    p p.size mask
    #xFF xor and   
    nfdrop ;

: buffer.clear | base p mask |
    0 p p.read !
    0 p p.write ! ;

\ macros not using the expanded >z procedures

: buffer.empty? buffer.empty>z z? ;    
: buffer.room?  buffer.room>z z? ;    


\ transfers: assuming there is a 'yield' operation (which might be a
\ busy loop) a transfer macro can be constucted.

: buffer.read/yield | b p m |
    begin b p m buffer.empty? while yield repeat
    b p m buffer.read ;

: buffer.write/yield | b p m |
    begin b p m buffer.full? while yield repeat
    b p m buffer.write ;


    
\ constructors
    
\ : compile-buffer pre- base mask |
    
    

    
forth
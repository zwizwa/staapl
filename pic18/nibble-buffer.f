\ A static 15 byte buffer queue.

\ A buffer consists of:
\   - a variable containing [ read | write ] pointers (nibbles)
\   - base pointer (one 16 bit line of ram)

\ The idea is to use a set of macros to instantiate specialized words
\ that read and write from a buffer + macros for empty? and full?
\ status.


macro
: buffer.read  | RW base |
    base 2 lfsr
    RW @ rw-read RW ! ;

: buffer.write | RW base |
    base 2 lfsr
    RW @ rw-write RW ! ;

: buffer.empty? | RW base |
    RW @ rw-empty>z z? ;

: buffer.full? | RW base |
    RW @ #x10 - rw-empty>z z? ;
: buffer.size  | RW base |
    RW @ rw-size ;
forth

\ Re-usable core routines for the macros above. These assume the a
\ register is set to the base of the buffer.

: rw-read   \ RW -- byte RW+
    dup >x
    #xF0 and swap-nibble
    al +! @a+
    x> #x10 + ;
    
: rw-write  \ byte RW -- RW+
    dup >x
    #x0F and al +! !a+
    x> 1 + ;

: rw-size \ RW
    dup swap-nibble - #x0F and ;
    
    
: rw-empty>z \ RW
    rw-size
    nfdrop ;    


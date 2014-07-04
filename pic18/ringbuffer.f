
\ create a circular buffer in a 16 byte RAM line.

\ 1 state var, 1 guard byte. state vars are packed: high nibble =
\ read, low nibble = write the created word loads the a reg with the
\ buffer body. these words need current buffer in a reg.  

\ a -> [ read | write ], 15 buffer bytes

\ in order to distinguish empty from full, one guard byte is used. so 14 = full



: buffer:read
    @a #xf0 and nswap @i         \ fetch next item
    @a #x10 + c? if #x10 + then  \ update ptr
    !a ;                         \ store
: buffer:write
    @a #x0f and !i                          \ store next item
    @a nswap #x10 + c? if #x10 + then nswap \ update ptr
    !a ;

: buffer:clear  #x11 !a ;  \ clear buffer


: buffer-size \ [ read | write ]
    dup #xF0 and >x
    nswap #xF0 and
    x>  -
    nc? if #x10 - then nswap ;   DOES-NOT-WORK
    
: buffer:size  @a buffer-size ;


\ simple fast ringbuffer


variable read-ptr
variable write-ptr
#x0F constant buffer-mask  

: buffer:read
    read-ptr @ buffer-mask and i@  \ fetch
    1 read-ptr +! ;                \ increment

: buffer:write
    write-ptr @ buffer-mask and i! \ store
    1 write-ptr +! ;
    
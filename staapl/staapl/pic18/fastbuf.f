\ simple fast ringbuffer

\ more like an example since it's all early bound and i don't have a
\ proper way to abstract this kind of stuff.. if you need only one,
\ just load the file.

\ needs the following symbols defined. uses indexed addressing for the
\ buffer bulk.

\ variable buffer-read-ptr
\ variable buffer-write-ptr
\ #x0F constant buffer-mask

macro

: buffer-wrap
    buffer-mask and ;

: buffer-ptr@+
    @ postinc buffer-wrap ;

: buffer-read
    read-ptr  buffer-ptr@+ @i ;

: buffer-write
    write-ptr buffer-ptr@+ @i ;

: buffer-size
    write-ptr @
    read-ptr  @  - buffer-wrap ;
    
forth



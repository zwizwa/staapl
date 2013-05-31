\ serial terminal I/O with XON/XOFF

\ to keep things simple, we will tokenize on receive. all dictionary
\ words will be truncated to (let's say) 8 characters, so there's no
\ need for large buffers. so:

\ * 'read' which read in a single word, delimited by whitespace.
\ * leading whitespace is removed
\ * flow control is per word: X is always OFF outside of 'read'
\ * I/O is half duplex

\ Wikipedia says:

\ the XON/XOFF protocol is controlled by the recipient of the data
\ being transferred. receiver sends XOFF when it can't take any more
\ data, and sends XON when it's ready again.


19 constant XOFF \ #x13
17 constant XON  \ #x11


\ the constants 'read-buffer' and 'read-size-mask' need to be
\ defined. the buffer is a standard forth string: first byte is
\ size. to simplify the buffer needs to be aligned and a power of 2
\ size.

\ buffer code. the 'a' register contains buffer base. if buffer is
\ full, data is not written.

: a->read-buffer
    #xA0 #x00 a!! ;

: read-size-current
    al @ read-size-mask and ;
  
: read-record  \ byte --
    read-size-current
    read-size-mask
      = if drop ; then  \ ignore because full
    !+a ;               \ store in buffer

: read-finish \ --
    read-size-current
    a->read-buffer !a ; \ store size byte

\ interpret backspace and special characters    
: read-record/interpret
    read-record ;


: read
    XON transmit read-word
    XOFF transmit ;


\ zero terminated list of whitespace chars
: f->whitespaces  f->  9 , 10 , 13 , 0 ,
  
: white? \ char - char ?
    f->whitespaces
    begin
	@f++
	z? if drop 0 ; then   \ exhausted: no whitespace
	=? if drop -1 ; then  \ found whitespace
	drop
    again ;
  
: read-word
    a->read-buffer

    \ state A: skip leading whitespace
    receive
    white? if
	drop read-word ;
    then
    read-record/interpret

    \ state B: whitespace delimits word
    begin
	receive
	white? if
	    drop  \ whitespace ends
	    read-finish ;
	then
	read-record/interpret
    again ;

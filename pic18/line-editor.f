staapl pic18/stdin
staapl pic18/stdout
staapl pic18/vector
staapl pic18/cond
staapl pic18/compose-macro



: terminal> i> ;
: >terminal
    \ dup p
    >o o-flush ;    


variable stringi
macro
: string-index-max 31 ; \ bufsize - 1 (room for null terminator)
forth
  
  
: string-index   stringi @ ;

: a!string       0 2 a!! ; \ max 254 + 1 bytes, minus page alignment
: a!string-endx  a!string string-index al +! ;
: a!string-endx+ a!string-endx stringi 1+! ;
: >string        a!string-endx+ >a ;
: string-last    a!string-endx al 1-! a> ;
    
\ : p-string       a!string string-index 1 - dup 0 = if ; then for a> emit next cr ;

: input-keys
    begin
        input-key
        string-index 0 = not if
            string-last 0 = if ; then
        then
    again
    
: line-editor
    0 stringi !
    input-keys
    string-index 1 -  \ don't count null
    \ p-string
    ;

\ print space to erase character
: backspace \ char --
    string-index 0 = if drop ; then
    drop 8 >terminal
    32 >terminal
    8 >terminal
    stringi 1-! ;

    
: input-key
    terminal>
    \ dup px
    \ carriage return: also print line feed
    dup 13 = if
        10 >terminal >terminal
        0 >string ;
    then

    \ BS, DEL
    dup 8   = if backspace ; then
    dup 127 = if backspace ; then

    \ ignore all ANSI escape codes
    dup 27  = if
        \ ignore escape codes
        \ http://en.wikipedia.org/wiki/ANSI_escape_code
        drop
        terminal>
        dup #x5B = if
            \ CSI se terminated by char in 64-127 range
            begin drop terminal> 1st 6 high? until \ CSI
            drop ; then
        \ Otherwise it's a 2-byte code
        terminal> drop \ px
        terminal> drop \ px
        ;
    then

    \ ignore all non-ascii characters
    dup 32  <  if drop ; then
    dup 127 >= if drop ; then
    
    \ print a '!' in case the buffer is full
    string-index string-index-max = if
        drop 
        33 >terminal
        8  >terminal ;
    then
    
    \ record & echo
    dup >string >terminal ;


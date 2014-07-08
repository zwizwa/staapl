staapl pic18/stdin
staapl pic18/stdout
staapl pic18/vector
staapl pic18/cond
staapl pic18/compose-macro


\ FIXME: put this somewhere else
: usb>io
    [ stdin -> console-EP OUT> ] i/c
    [ stdout -> console-EP >IN ] i/c
    [ stdout-flush -> console-EP IN-flush ] i/c ;

: terminal> i> ;
: >terminal
    \ dup p
    >o o-flush ;    


variable string-index/ready
macro
: string-buf-size 10 ;
: string-ready    string-index/ready 0 ;
forth
  
  
: string-index   string-index/ready @ >> ;
: a!string       0 2 a!! ; \ 128 byte buf
: a!string-endx  a!string string-index al +! ;
: a!string-endx+ a!string-endx 2 string-index/ready +! ;
: >string        a!string-endx+ >a ;
: backspace      2 string-index/ready -! ;    

: p-string       a!string string-index dup 0 = if ; then for a> emit next cr ;

: line-editor
    0 string-index/ready !
    begin input-char string-ready high? until
    string-index
    p-string
    ;

: input-char    
    terminal>
    \ carriage return: also print line feed
    dup 13 = if
        10 >terminal >terminal
        string-ready high ;
    then

    \ backspace: print space to erase character
    dup 8 = if
        string-index 0 = if drop ; then
        >terminal 32 >terminal 8 >terminal
        backspace ;
    then

    \ ignore all ANSI escape codes
    dup 27  = if
        \ ignore escape codes
        drop
        terminal> drop
        terminal> drop ;
    then

    \ ignore all non-ascii characters
    dup 32  <  if drop ; then
    dup 127 >= if drop ; then
    
    \ print a '!' in case the buffer is full
    string-index string-buf-size = if
        drop 
        33 >terminal
        8  >terminal ;
    then
    
    \ record & echo
    dup >string >terminal ;


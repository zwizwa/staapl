staapl pic18/stdin
staapl pic18/stdout
staapl pic18/vector
staapl pic18/cond
staapl pic18/compose-macro


\ FIXME: put this somewhere else
: usb>i   stdin -> console-EP OUT> ;
: usb>o   stdout -> console-EP >IN ;
: usb>o/f stdout-flush -> console-EP IN-flush ;
    
: usb>io
    usb>i
    usb>o
    usb>o/f ;

    

: ~>o  dup emit >o o-flush ;
    
: io-echo
    begin
        i>
        dup 13 = if 10 ~>o then
        ~>o
    again
        
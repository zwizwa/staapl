\ debug routines

staapl pic18/compose-macro
staapl pic18/stdin
staapl pic18/string
staapl pic18/afregs
staapl pic18/vector

\ Host needs to clobber a reg for memory accesses so can't get at it
\ without us providing the value like this.  For symmetry, also do f.
: a@@   al @ ah @ ;
: f@@   fl @ fh @ ;    
  

\ Resume recursive interpreter call, e.g. after 'break'.  The
\ interpreter at the point of the executed command is 2 levels deep:
\ one for the call from the 'interpreter' loop, and one for the call
\ from 'jsr/ack'.  The rest are tail calls.  If there is no previous
\ stack frame, don't do anything.

: continue  STKPTR @ 3 >= if pop pop then ;


\ After issuing RPC packet, fall into interpreter.  During the RPC
\ call the host can use us as a resource.  When host is done it will
\ issue 'continue' to break the interpreter loop.
: tx-end-rpc tx-end interpreter ;
  
  
\ --- Host-assisted commands ---

    
\ Send RPC command token and payload bytes from stdin.
    
: i:rpc \ n token --
    >r dup
    1 + tx-begin-rpc     \ start RPC command packet
    r> transmit          \ send token
    for i> transmit next \ send payload
    tx-end-rpc ;

: i:list>h
    0 i:rpc ; \ n --
    
: i:fcmd \ lo hi --
    af>r       \ Let host clobber a&f during execution of command.
    fstring>i  \ Pascal string from flash
    1 i:rpc    \ see host-rpc / host-rpc-cmd in tethered.rkt  (uses `live:')
    r>af ;


    

    
    

\ Send list from different streams, restoring user's stdin.
: alist>h i>r a>i i:list>h r>i ; \ RAM
: flist>h i>r f>i i:list>h r>i ; \ Flash
: dlist>h i>r d>i i:list>h r>i ; \ datastack

: 1list>h 1 dlist>h ;                  \ 1 byte

: fstring>h i>r fstring>i i:list>h r>i ; \ Flash Pascal string (== size prefixed)    
: fstring>i f!! f>i i> ;                 \ lo hi -- n

: fcmd i>r i:fcmd r>i ;   

    
\ Execute host command <cmd> as ` <cmd> host
macro
: host sym fcmd ;
forth


\ send out current execution point.
: trace
    xh @ xl @ 2 dlist>h
    ` htrc host ;

\ For sending log data from target to host, it seems simplest to just
\ send a "pull" command to host and let it control the data transfer,
\ instead of pushing the data in a single packet and calling a host
\ word.  Dictionaries scanned are: host, scat, and scheme
    
    
\ Operations on list on host stack.
: hlp   ` hlp   host ;  \ host list print
: hlpx  ` hlpx  host ;  \ host list print hex
: hlpxa ` hlpxa host ;  \ host list print hex + ascii

: >h    ` t> host ;  \ Byte to host stack

: emit  i>r 1list>h hlp r>i ;
: adump alist>h hlpxa ;
: fdump alist>h hlpxa ;    
    
macro : .sym     sym .fstring ;
forth : .fstring fstring>h hlp ;

\ : rst ack
\     UCON USBEN low \ turn off USB machinery
\     0 for next
  \     reset ;


\ Host commands operating on target stack / memory directly.    
: ts  ` ts  host ;
: p   ` p   host ;
: px  ` px  host ;
: _p  ` _p  host ;
: _px ` _px host ;


\ misc tools
    
\ print data and retain stack pointers    
: psps  FSR0L @ px FSR1L @ px cr ; 
: cr    #x0A emit ;  
  
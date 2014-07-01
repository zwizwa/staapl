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

: continue  STKPTR @ 3 _>= if pop pop then ;


\ After issuing RPC packet, fall into interpreter.  During the RPC
\ call the host can use us as a resource.  When host is done it will
\ issue 'continue' to break the interpreter loop.
: tx-end-rpc tx-end interpreter ;
  
  
\ --- Host-assisted commands ---

    
\ Send RPC command token and payload bytes from stdin.
    
: rpc \ n token --
    >r dup
    1 + tx-begin-rpc     \ start RPC command packet
    r> transmit          \ send token
    for i> transmit next \ send payload
    tx-end-rpc ;

: dump
    0 rpc ; \ n --
    
: fcmd \ lo hi --
    af>r       \ Let host clobber a&f during execution of command.
    fstring>i  \ Pascal string from flash
    1 rpc      \ see host-rpc / host-rpc-cmd in tethered.rkt  (uses `live:')
    r>af ;

: fstring>i \ lo hi -- n
    f!! f>i i> ;
    

\ Connect stdin to different streams   
: adump a>i dump ; \ RAM
: fdump f>i dump ; \ Flash
: ddump d>i dump ; \ datastack
: 1dump 1 ddump ;  \ 1 byte

: fstring fstring>i dump ; \ Like fdump but send Pascal string (== size prefixed)    

\ Convenient macro for compiling symbolic word to a Flash string (as a
\ word that saves the address of the string in the f register) and
\ invoking fcmd.  To execute kb host command, do: ` kb host

\ sym>f overwrites the f register, so we save it.  Might as well save
\ the a register here so host can clobber it during the RPC call,
\ avoiding an expensive round-trip delay for host to save/restore the
\ reg.
   
macro
: host sym fcmd ;
forth


\ send out current execution point.
: trace
    xh @ xl @ 2 ddump
    ` trc host ;

\ For sending log data from target to host, it seems simplest to just
\ send a "pull" command to host and let it control the data transfer,
\ instead of pushing the data in a single packet and calling a host
\ word.
    
\ Some wrapped host commands.  These operate on the host stack.
: pb ` pb host ;
: ph ` ph host ;
: >h ` t> host ;  \ Byte to host stack
: ts ` ts host ;    

: emit     1dump pb ;
: hd       1dump ph ;
: .fstring fstring pb ;   

macro
: .sym  sym .fstring ;
forth


\ FIXME: words like _>= should go into a library, or at least "if"
\ should be able to interpret real stack words so predicates can be
\ implemented as functions.
macro    
: _>= - nfdrop c? ;
forth     


\ : rst ack
\     UCON USBEN low \ turn off USB machinery
\     0 for next
\     reset ;
  
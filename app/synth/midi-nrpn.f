\ NRPN state machine  
variable nrpn-addr-lo
variable nrpn-addr-hi
variable nrpn-val-lo
variable nrpn-val-hi
: nrpn-val15 \ -- lo hi | convert to 15-bit value
    nrpn-val-lo @ rot<<
    nrpn-val-hi @ ;
  
\ NRPN CC
: CC63 nrpn-addr-hi ! ; 
: CC62 nrpn-addr-lo ! ; 
: CC06 nrpn-val-hi ! ;
: CC26 nrpn-val-lo !
    \ This is optional in midi spec, though we require it to start
    \ transaction
    commit-nrpn ; 


\ Insert jump to compiled macro.  Easy way to work around long jump
\ addresses in route tables.
macro : .. i/c . ; forth  
    
\ low-level synth parameters are available through NRPN.
: commit-nrpn
    \ psps
    nrpn-addr-hi @ 0 = not if ; then \ ignore
    nrpn-val15
    nrpn-addr-lo @
    \ ` nrpn: .sym ts
    1 - 4 min
    \ ts
    route
        [ _p0    ] ..  \ 1
        [ _p1    ] ..  \ 2
        [ _p2    ] ..  \ 3
        [ _synth ] ..  \ 4
        _drop          \ ignore rest
        \ ` ignore:nrpn: .sym print-nrpn
        ;

\ : print-nrpn a>r nrpn-addr-lo 0 a!! 2 for a> a> _px next cr r>a ;

\ Setting the full parameter set requires 3 nrpn messages.  Each nrpn
\ message is 4 midi messages so in total 48 = 3 x 4 x 4 bytes are
\ required to set all params, which fits in a single USB packet.
        
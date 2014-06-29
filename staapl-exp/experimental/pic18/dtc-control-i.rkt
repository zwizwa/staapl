#lang staapl/pic18 \ -*- forth -*-
provide-all

\ On-target immediate words implementing the control words.
staapl pic18/double-math
staapl pic18/double-pred
staapl pic18/execute
staapl pic18/dtc


\ This needs "comma" and a way to back-patch words.  The idea is to
\ compile to a RAM buffer first, and transfer it to FLASH when it's
\ done.
staapl pic18/double-comma

macro
: _address  word-address lohi ;
forth  

: _mask    #x3F and ;
: _lmask   _mask #x40 or ;
: _compile _mask _, ;  \ takes word address as 2 bytes
: _literal _lmask _, ;
: _0       0 0 ;
    
\ These compile unconditional and conditional jump.
: _jump,   ' _run    _address exitbit _compile ;
: _0=jump, ' _0=run; _address         _compile ;


\ Jumps are proper primitives.  They take a single argument which we
\ compile as a literal.
: _hole    _here@ _0 _literal ;
: _lpack   _>> _lmask ; \ pack byte address as literal
: _then    _>r _here@ _lpack _r> _! ;  \ patch hole
    
: _if      _hole _0=jump, ;
: _else    _>r _hole _jump, _r> _then ;

: _begin   _here@ ;
: _again   _lpack _, _jump, ;
: _until   _lpack _, _0=jump, ;    

    
\ COMPLICATIONS: because of the exit bit, jump targets need to be
\ protected so the previous instruction doesn't get exit-tagged.  See
 \ -m.ss
    

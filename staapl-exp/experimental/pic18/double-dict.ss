#lang staapl/pic18 \ -*- forth -*-
provide-all

\ Dictionary compiler.  Compiles to RAM before transferring a string
\ of threaded code to FLASH.

staapl pic18/double-math
staapl pic18/conditionals
staapl pic18/prom

2variable here  \ Current code compilation point.
2variable last  \ Last uploaded code endx.
2variable head  \ Dictionary head.

: _here here 0 ; 
: _last last 0 ;
: _head head 0 ;    

    
    
macro
: here-start #x100 ;  
: here-mask  #x3F ;   \ one RAM block
forth


\ We compile to RAM, then transfer lines to FLASH memory.  The RAM is
\ filled with an inert Flash AND mask to be able to overlay already
\ flashed code.
  
: prepare
    _here@ _last _!
: clear-cbuf
    here-start 2 lfsr
    here-mask 1 + for
        #xFF !a+  \ inert Flash AND mask
    next ;

: program-line \ _addr - 
    _dup  fh ! fl !
    _cbuf ah ! al !
    8 for @a+ !f+ next
    flash program ;

: line >r #xF8 and r> ;          \ convert pointer -> line pointer
: line-down >> >> >> ;           \ byte -> line addr (round down)
: line-up   1 - line-down 1 + ;  \                          up
: nb-lines
    here @ line-up
    last @ line-down - ;

: _8 8 0 ;
    
: finish
    _last _@ line   \ start line
    nb-lines for
        _dup program-line _8 _+
    next _drop ;
    
\ Convert address to compile buffer address.    
: _cbuf
    drop
    here-mask and
    here-start 8 >>> ;
    
: _here@ _here _@ ;
: _cell+ _1+ _1+ ;    
: _,
    _here@ _cbuf _!           \ store to scratch buffer
    _here@ _cell+ _here _! ;  \ update pointer

\ @ and ! use RAM directly.  Flash needs a separate access word.
: addr>a ah ! al ! ;    
: _@ addr>a : _a@+ @a+ @a+ ;
: _! addr>a : _a!+ >r !a+ r> !a+ ;    

\ : _flash@ fh ! fl ! @f+ @f+ ;



#lang staapl/pic18 \ -*- forth -*-
provide-all

staapl pic18/double-math
staapl pic18/double-pred
staapl pic18/execute


\ A direct threaded Forth using native code primitives.  NEXT is
\ procedure return to an explicit interpreter loop.

: _IP!
    fh ! fl ! ;
: _exit
    r> fh !      
    r> fl ! ;  
: _dolit
    @f+ @f+ ;    
: _jump
    @f+ fl !
    @f+ fh ! ;
: _0jump
    or z? if drop _jump ; then
    drop @f+ drop @f+ drop ;
    
: enter 
    fl @ >r
    fh @ >r   
    TOSL fl @!  \ TOS cannot be movff dst, but src is ok       
    TOSH fh @!
    pop ;   
  
: continue
    begin @f+ @f+ execute/b again

: _bye
    _exit  \ remove DTC continuation
    pop ;  \ break the "continue" loop


  
  
    
\ Trampoline entry from native code.  The 'interpret' word will run a
\ dtc primitive or primitive wrapped program.
    
    
: bye>r
    enter
    ' _bye compile ;
: interpret     \ ( lo hi -- )
    bye>r       \ install continuation into dtc code "bye ;"
    execute/b   \ invoke the primitive (might be enter = wrapped program)
    continue ;  \ invoke threaded continuation


\ Return stack
    
: _>r    >r >r ;
: _r>    r> r> ;    
: _rdrop rdrop rdrop ;
: _r     rl rh ;

    
        
    
macro
: _if    make-label dup ' _0jump compile , ;
: _then  label: ;
forth


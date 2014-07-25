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



\ Return stack
: _>r    >r >r ;
: _r>    r> r> ;    
: _rdrop rdrop rdrop ;
: _r     rl rh ;

    
    
\ Macros to create DTC words in staapl code.
\ To compile from outer interpreter:
\  _foo is a macro then use as is
\  _foo is a target word then replace with:  ' _foo _compile
macro
: _compile  i word>m m> ,, ;
: _literal  >m ' _dolit _compile m> ,, ;     
: _if       ' _0jump _compile make-label dup >m ,, ;
: _then     then ;  \ note that end: is called here  (see compiler-unit.rkt)
forth
    
    
\ Trampoline entry from native code.  The 'interpret' word will run a
\ dtc primitive or primitive wrapped program.
    
    
: bye>r
    enter
    ' _bye _compile ;
: interpret     \ ( lo hi -- )
    bye>r       \ install continuation into dtc code "bye ;"
    execute/b   \ invoke the primitive (might be enter = wrapped program)
    continue ;  \ invoke threaded continuation


        
    


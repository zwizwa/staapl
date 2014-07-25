staapl pic18/double-math
staapl pic18/double-pred
staapl pic18/execute
staapl pic18/mem16
staapl pic18/cond
staapl pic18/compose-macro


\ A direct threaded Forth using native code primitives.  NEXT is
\ procedure return to an explicit interpreter loop.

\ This is a DTC variant, so no execution from RAM on the PIC.
\ Execution tokens are 16 bit Flash byte addresses.

\ Going for simple code here.  Can be made faster.


: IP@    fl @ fh @ ;  \ -- lo hi
: IP!    fh ! fl ! ;  \ lo hi --
: _exit  _r> IP! ;    \ --
: fetch  @f+ @f+ ;    \ -- lo hi
: ~jump   fetch IP! ;  \ --
: 0jump  or nfdrop z? if ~jump ; then fetch _drop ; \ lo hi --
    
: enter
    IP@ _>r
    TOSL fl @!  \ TOS cannot be movff dst, but src is ok       
    TOSH fh @!
    pop ;   
  
: continue
    begin fetch execute/b again

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
: cw>xt     word>m m> 2 * ;
: _compile  i cw>xt ,, ;
: _literal  >m ' fetch _compile m> ,, ;     
: _if       ' 0jump _compile make-label dup >m ,, ;
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


        
macro
: lohi | x | x x 8 >>> ;  
: idtc i/c cw>xt lohi interpret ;
forth

: test1 [ enter #x1234 _literal ' _dup _compile ' _exit _compile ] idtc ;

: ~test2 enter _if 1 _literal _then ' _exit _compile
: test2 ' ~test2 idtc ; \ works for nonzero argument, not for zero

: ~test3 enter _if 1 _literal ' _exit compile _then 2 _literal ' _exit _compile
: test3 ' ~test3 idtc ; \ doesnt work
    

    
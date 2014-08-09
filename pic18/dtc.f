staapl pic18/double-math
staapl pic18/double-pred
staapl pic18/execute
staapl pic18/cond
staapl pic18/compose-macro
staapl pic18/afregs

\ A direct threaded Forth using native code primitives.  NEXT is
\ procedure return to an explicit interpreter loop.

\ This is a DTC variant, so no execution from RAM on the PIC.
\ Execution tokens are 16 bit Flash byte addresses.

\ Going for simple code here.  Can be made faster.


: IP@    fl @ fh @ ;  \ -- lo hi
: IP!    fh ! fl ! ;  \ lo hi --
: _exit  _r> IP! ;    \ --
: fetch  @f+ @f+ ;    \ -- lo hi
: jump   fetch IP! ;  \ --
: 0jump  or nfdrop z? if jump ; then fetch _drop ; \ lo hi --
    
: enter
    IP@ _>r
    TOSL fl @!  \ TOS cannot be movff dst, but src is ok       
    TOSH fh @!
    pop ;   
  
\ Return stack
: _>r    >r >r ;
: _r>    r> r> ;    
: _rdrop rdrop rdrop ;
: _r     rl rh ;


\ Memory.  Save f because it's used in the interpreter, but all
\ primives are allowed to clobber a.  DTC doesn't expose it.
\ Raw access:
: _rom@ f[ IP! fetch ]f ;
: _ram@ ah ! al ! @a+ @a+ ;
: _ram! ah ! al ! >r !a+ r> !a+ ;
\ Highlevel acces:    
\ 0000-0FFF RAM/SFR
\ 1000-FFFF ROM
: _@    #xF0 + nc? if _ram@ ; then #x10 + _rom@ ;
: _!    #xF0 + nc? if _ram! ; then _2drop ;
    
    
    
\ Macros to create DTC words in staapl code.
\ To compile from outer interpreter:
\  _foo is a macro then use as is
\  _foo is a target word then replace with:  ' _foo _compile
macro
: cw>label word>m m> ;
: label,   >m 2 m> * ,, ; \ store byte addresses
: _compile i cw>label label, ;
: _literal >m ' fetch _compile m> ,, ;
: _if      ' 0jump _compile make-label dup >m label, ;
: _else    '  jump _compile make-label dup >m label, m-swap _then ; 
: _then    then ; \ includes end:
: _begin   begin ; \ includes enter:
: _again   ' jump _compile m> label, ;
: _until   ' 0jump _compile m> label, ;
: _do      _begin ;
: _while   _if ;
: _repeat  m-swap _again _then ;     
forth
    
\ Trampoline entry from native code will run a dtc primitive or
\ primitive wrapped program as "<xt> bye"
: execute/dtc \ ( lo hi -- )
    bye>r \ push original IP, IP=bye
    execute/b \ invoke the XT (primitive or high level word's enter)
    thread-loop ;
: thread-loop \ execute token thread
    begin fetch execute/b again
: bye>r
    enter
    ' _bye _compile ;
: _bye
    _exit \ pop original IP
    pop ; \ break out of thread-loop

\ Multiplication

load mul16.f  
variable X0 variable X1
variable Y0 variable Y1
variable R0 variable R1 variable R2 variable R3 variable R4

: XY!  Y! X! ;
: R01@ R0 @ R1 @ ;   
: R23@ R2 @ R3 @ ;   
: _*   XY! umul R01@ ;
: _um* _* R23@ ;


\ for s/u Y is unsigned = top argument
    
    
\ Fractional multiplication is signed x unsigned -> signed
\ Seems most useful for DSP.
: _*>>16  XY! sumul R23@ ;
: _*>>8   XY! sumul R1 @ R2 @ ;

\ fixme : also do signed and fixedpoint

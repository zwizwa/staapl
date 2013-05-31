#lang planet zwizwa/staapl/pic18 \ -*- forth -*-
provide-all

\ Macros implementing the control words.  For a self-hosted
\ interpreter these need to be replaced by immediate words.

staapl pic18/double-math
staapl pic18/double-pred
staapl pic18/execute
staapl pic18/vm-core

macro

\ note: XT need to be word addresses, since i have only 14 bit
\ literals. return stack still contains byte addresses though, so for
\ now it's kept abstract.



\ create a jump label symbol and duplicate it (for to and from)
: 2sym>m      sym >m m-dup ;

\ jumps are implemented as literal + primitive (instead of reading
\ from instruction stream)
    
: m>jmp    m> literal ' _run     compile _exit ;
: m>0=jmp  m> literal ' _0=run;  compile  ;    
    
: _begin    2sym>m m> label: ;   \ back label
: _again    m>jmp ;              \ back jump
: _until    m>0=jmp _space ;     \ conditional back jump
    
: _if     2sym>m m>0=jmp ;                  \ c: -- label1
: _else   2sym>m m>jmp m-swap m> label: ;   \ c: label1 -- label2
: _then   m> label: _space ;        \ c: label --

: _space  ' _nop compile ; \ necessary when 'return' needs to be isolated.
    
\ : _for    _2sym>m m> label ' do-for compile ; \ c: -- label
\ : _next   _m>literal ' do-next compile _space ;

: _for
    ' _>r compile
    _begin ;
: _next
    ' do-next compile  m>0=jmp
    ' _rdrop compile
    _space ;

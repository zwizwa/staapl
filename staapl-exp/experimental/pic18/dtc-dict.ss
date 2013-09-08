#lang staapl/pic18 \ -*- forth -*-
\ Taken out of dtc.ss : no on-target dictionary & compiler

staapl pic18/double-dict


\ Bootstrap compiler words.
macro
: compile  address ,, ;
: literal  >m ' _dolit compile m> ,, ;
: _;       ' _exit compile ;
forth

\ On-target compiler words.
macro
: >xt  address lohi ;
: _,   ,, ;
forth

  
: _compile  _, ;
: _literal  ' _dolit >xt _, _, ;
  

\ Immediate words

: _0  0 dup ;
: _0jump,  ' _0jump >xt _, ;
: _jump,   ' _jump >xt _, ;    
: _hole,   _here@ _0 _, ;
    
: _begin   _here@ ;
: _again   _jump, _, ;
: _until   _0jump, _, ;

: _if      _0jump, _hole, ;
: _then    _>r _here@ _r> _cbuf _! ;
: _else    _>r _jump, _r> _hole, ;


\ Dictionary meta data.

\ For now we compile the dictionary meta data inline with the code so
\ we can reuse compilation primitives.  Later the dictionary could be
\ separated.

\ Compile a pstring to the dictionary.    
: compile-name \ lo hi --
    _dup _@ drop  \ lo hi nb.bytes
    >> 1+         \       nb.words
    for _dup _@ _, _cell+ next
    _drop ;

\ Create a new dictionary entry.  Format: [ next XT CT name code ]
    
: word \ lo hi --
    _here@  _>r         \ Save location of new head pointer.
    _head _@ _,         \ Compile link to old head.
    _r> _head _!        \ Store new head pointer.
    _here@ _>r _0 _,    \ Save XT hole + Compile stub.
    ' _compile >xt _,   \ Compile CT
    compile-name        \ Compile the name string.
    _here@ _r> _cbuf _! \ Patch XT hole.
    ;


    

\ String comparison.  Since one of them is in RAM and the other in
\ FLASH both pointers can be used to iterate over the strings.

: flash/ram-compare \ FH FL AH AL -- ?
    a!! f!! a/f-compare ;
    @a @f+ = if @a+ n-a/f-compare ; then 0 ; \ size needs to be equal
: n-a/f-compare \ len --
    1 - c? if drop -1 ; then     \ size = 0 => they are equal
    @a+ @f+ = if n-a/f-compare ; then \ recurse if character matches
    drop 0 ;

    
    

\ untested
    
\ Lookup a pstring in the flash dictionary.  Returns 0 if not found,
\ or a link to the dictionary record if found.
    
: find \ lo hi --
    _>r
    _last _@
: find-loop
    _dup or 0 = if _drop _rdrop _0 ; then \ Stop if it's zero
    _dup _r _@ flash/ram-compare if _rdrop ; then \ Compare and exit if found 
    _@ find-loop ; \ Deref and continue.
    
    
    
    
: init-dtc
    #x1000 lohi _here ! _0 _last !
    ;

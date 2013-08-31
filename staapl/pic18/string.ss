#lang staapl/pic18 \ -*- forth -*-
provide-all
staapl pic18/compose-macro

\ loads the address following the call to 'f->' into the stack, and
\ returns to the parent word. this allows flash buffers/strings to be
\ implemented as ordinary words

\ : mystring  fstring->  0 , 1 , 2 , 

: table-> \ -- lo hi 
    TOSL @
    TOSH @
    pop ;

\ It seems best to not use the abstraction above, but save the address
\ on the datastack instead.
    
    
\ Emulate quoted symbols by compiling them into Flash and storing the
\ contents of a Pascal string in the f register.
macro

: pascal sym>bin dup l:length swap l:cons ;
  
: sym \ sym -- lo hi
    >macro                            \ convert quoted symbol to macro
    [ >m table-> m> pascal ' , bin, ] \ macro tail that implements Flash bin word.
    compile-call-macro/exit ;         \ glue together 2 parts, compile + call compiled word
forth
  

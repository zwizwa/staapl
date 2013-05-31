\ FIXME: re-implemented as vector.f : basically the same, just using
\ standard variables instead of a separate hook page.

\ this is a simple mechanism for changing run-time behaviour. i know
\ at least 3 distinct forth mechanisms that implement this:

\ DEFER IS
\ VARIABLE EXECUTE @ !
\ DOER MAKE

\ the mechanism here is a variant of brodie's DOER MAKE (see Thinking Forth)

\ i try to keep it as simpel as possible.

\ 1. one hook table, enough for 128 hooks. hook identified by byte.
\ 2. no read functionality
\ 3. write functionality ala DOER MAKE

\ implementation: macros (so LFSR can be used) + clobbers a reg.


\ FIXME: this is hardcoded, do automatic alloc


macro
: hook-page 1 ;  \ hooks will be stored here
: ->       hook-page a!! !a-hook ; \ important word, so short rep
: run-hook hook-page a!! @a-hook ;
forth

: !a-hook
    TOSL @ !a+
    TOSH @ !a+
    pop ;

: @a-hook
    push
    @a+ TOSL !
    @a+ TOSH ! ;
  
\ EXAMPLE

\ 0 5  2constant bar

\ : foo  bar -> 1 2 3 ;
\ : baz  bar run-hook ;    


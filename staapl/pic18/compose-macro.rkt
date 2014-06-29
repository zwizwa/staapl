#lang staapl/pic18 \ -*- forth -*-
provide-all

\ Some tools for composing / compiling macros.
macro

\ Instantiate a macro and compile a call to it.  Useful for
\ abstractions that use the execution stack to get at Flash addresses,
\ like Flash strings.

\ Beware that the code compiled by macro->label/raw only has an entry
\ point and does NOT append `exit' to allow for user-defined exits,
\ (e.g. f->)
: macro->label/raw \ macro -- label
    make-label dup >m swap compile-macro/raw m> ;

\ Convert macro to compiled code and compile a call to it, extending
\ the macro with exit code.
: compile-call-macro/exit \ macro exit-macro --
    compose-macro macro->label/raw cw ;

\ For normal uses this is probably what's most useful.  Like i, but
\ compile as call, e.g. for jump tables.
: i/c [ exit ] compile-call-macro/exit ;
    
forth
  

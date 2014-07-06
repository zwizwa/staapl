#lang staapl/pic18 \ -*- forth -*-
provide-all

\ Some tools for composing / compiling macros.

\ These expose the ragged edge between Forth, the low-level language,
\ and the idea of highlevel concatenative languages.  The former is
\ really just a machine language while the latter has first class
\ code.  In Staapl it's possible to at least have concatenative
\ macros, but dumping them into code requires explicit management of
\ call and exit.

\ These are the two "higlevel" DWIM cases for interpreteting quoted macros.
\ Such quotations come from:
\   - named quotation: ' <name>
\   - anonymous code:  [ <word> ... ]

\ i : Inline the code here.
\
\ i/c : Compile the code somewhere else and insert a call to it.
\       An explicit 'exit' is appended to the body of the macro.



macro

\ Instantiate a macro and compile a call to it.  Useful for
\ abstractions that use the execution stack to get at Flash addresses,
\ like Flash strings.

\ Beware that the code compiled by macro->label/raw only has an entry
\ point and does NOT append `exit' to allow for user-defined exits,
\ (e.g. f->)
: raw-macro->label \ macro -- label
    make-label dup >m swap compile-macro/raw m> ;

\ Compile raw macro as call
: raw-i/c \ macro --
    raw-macro->label cw ;

\ For normal uses this is probably what's most useful.  Like i, but
\ compile as call, e.g. for jump tables.
: i/c [ exit ] compose-macro raw-i/c ;

    
forth
  

load synth.f   \ synth core
load debug.f
\ Disable interrupts during PK2 comm.  Note that it might be best to
\ use a packet approach: write a report to a region of ram and dump it
\ out periodically using nadump+

\ : _dump_ cli dup dump sti ;


\ Run the engine for a couple of seconds, then return control.  This
\ needs `init-board' and possibly `param' before it will work.

    
: runs \ seconds -
    engine-on
    for second next
    engine-off
    ;

\ 1 second busy delay
: second for 7 for #x100 for #x100 for next next next next ;


\ run a word with engine on
: 2execute \ lo hi --
    push TOSH ! TOSL ! ;

    
: play
    init-board
    engine-on
    2execute
    engine-off ;
    

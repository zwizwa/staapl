\ Startup code shared by monitor-serial.f monitor-icsp.f
\ The interpreter uses: init-warm, init-abort, init-quit for warm, abort, quit.

: init-abort
    #x80 init-ds       \ data stack
    #xC0 init-rs ;     \ retain stack (for byte-size >r r>, not the execution stack)

\ This needs to inline because it kills the exeuction stack.  Requires
\ parmeter stacks to be setup.
macro
: init-warm
    init-chip          \ chip specific, shared
    init-board         \ defined in board kernel file
    ;
: init-quit
    dup                \ init-xs overwrites WREG
    init-xs            \ execution stack
    drop ;

: init-cold

    \ Initialize from cold start.  First time around we need to setup
    \ XS to enable function calls.  Subsequent inlines/call of init-*
    \ need a working interactive system system (see 'quit', 'abort',
    \ 'warm')
    
    init-xs    \ return stacks (inline)
    init-abort \ data stacks
    init-board \ setup outputs and clock
    init-comm  \ serial interpreter comm init
;
    
forth


: _cold
    \ Install boot vector to jump here.
    ' _cold boot-vector!

    \ Cold start chip and VM.
    init-cold

    \ testi
    
    \ 0 for 0 for 0 for next next next testi

    interpreter ; \ FIXME
    
    \ Start if tether is attached
    interactive? if interpreter ; then
    
    \ Else fall into application..
    \ testi


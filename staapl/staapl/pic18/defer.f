\ deferred words

\ there is really no good solution here, since the problem i'm trying
\ to solve is really about coding style.

\ staapl forth uses an early binding strategy. what this means is that
\ as much as possible is fixed at compile time. combined with using
\ flash program storage, this makes programming code that can't
\ entirely use bottom up programming problematic. so

\ 1. early binding for optimized assembly code

\ 2. flash: write-once: no patching of code

\ 3. completely bottom up: code can only depend on earlier code, or
\    code defined in the same compilation unit.


\ this is usually ok for a finished product performing a well-defined
\ task. however, incremental developement often needs some plugin
\ (deferred) behaviour, otherwize you're stuck reloading code the
\ whole time. a good example is an interrupt service routine.

\ so, deferred words are very convenient. because of the restrictions
\ on flash memory, the only workable solution is to put addresses in
\ ram.

\ then remains the question on how to represent deferred
\ behaviour. the canonical way is to use code addresses. since the 18f
\ machines are 8bit, this is rather inconvenient. therefore i use an
\ indirect method. deferred behaviour is represented by 8bit execution
\ tokens (XTs), which are dereferenced using a ram table.

\ the ram table can be placed in a space which is not in the access
\ bank, so they don't compete with application global variables.

\ 1. XTs are 8bit
\ 2. table is defined in ram, preferrably in non-access memory
\ 3. calling clobbers the a register. (can be later removed)

\ some premature optimization urge: what about xt (functionality
\ encoded as byte) which does not need late binding (stored in flash)
\ ? this is already there in the form of 'route'


\ 2 constant xt-bank \ bank in multiple of #x80
: xt-bank 2 ;

: xt>word \ xt -- lo hi
    rot<< dup >r
    #xFE and      \ mask 1111 1110   low byte
    r> 1 and ;    \ mask 0000 0001   high byte

: xt>addr \ xt -- lo hi
    xt>word xt-bank + ;

: execute \ xt -- ...
    xt>addr
    
: 2execute \ lo hi --
    push TOSH ! TOSL ! ;

\ see mem.f
: @@ @ @a+ ;
: !! swap ! !a+ ;    
    
: run-hook \ var --
    @@ 2execute ;
    

\ that's the easy part.. the hard part is getting the addresses in
\ place. for this 'defer' is handy. an occurance of a word could
\ translate to 'XT execute'. but, maybe a more direct approach is
\ better: do something like 'xt bla' to reserve an execution token,
\ and implement it as a constant.

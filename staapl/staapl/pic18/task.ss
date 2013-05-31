#lang planet zwizwa/staapl/pic18 \ -*- forth -*-
provide-all

\ This file contains bare bones task switching code, in the form of
\ the following words. It switches the 3 stacks and the a and f
\ registers.
\
\    suspend  \ -- task      | copy state to data stack, return task pointer
\    resume   \ task --      | ignore current context, resume old task
\    swaptask \ ta var -- tb | swap task ta with task stored in var
\
\ For example, using only two tasks, a single variable is used to
\ store the task pointer of the other state.
\
\    variable other
\
\ Which enables the definition of a simple yield word
\
\    : yield suspend other swaptask resume ;
\
\ In general, one would call a 'schedule' word which transforms one
\ task id into another one. For example by placing one task in a
\ queue, and reading the next one from the queue.
\
\    : yield suspend schedule resume ;
\
\
\ Booting a task is very simple. It requires the initialization of the
\ 3 stack pointers to an available location in memory. This depends on
\ the particual memory model an is left to the user.
\
\  : spawn
\	suspend other !   \ suspend current task
\	#x10 rp !         \ move return stack pointer to half of stack
\	#x50 xp !         \ similar for the other 2
\	#x60 dp !
\       init-task ;
\ 



\ IMPLEMENTATION

macro
: dp FSR0L ;
: xp FSR1L ;
: rp STKPTR ;    
forth

\ A task is represented by a data stack pointer. The tricky bits are
\ dp@ and dp! which must not interfere with the WREG = TOP
\ optimization we have going on. It turns out this is easy:
\
\ dp@ is just dp @, which is implemented as 'dup', followed by w <- FSR0L
\ dp! is also just dp !, implemented as 'w -> FSR0L' followed by 'drop'
\
\ Note that saving of a / f registers and TABLAT is only necessary if
\ you use them in all tasks. the first 2-task app i did indeed used
\ both a and f reg in both tasks, so here's the safest version.

\ Note: this saves only the low byte of the FSR address, so task
\ stacks need to be in the same bank.

macro
: dp@  dp @ ;
: dp!  dp ! ;    
forth


macro
: suspend  \ -- task
    TABLAT @ fl @ fh @ al @ ah @ rp @ xp @ dp@ ;
: resume   \ task --
    dp! xp ! rp ! ah ! al ! fh ! fl ! TABLAT ! ;
: swaptask \ task var --
    swap! ;  
forth



\ YIELD: M.W 3b: to surrender or relinquish to the physical control of another 

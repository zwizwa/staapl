\ Problem: Keep track of active midi notes (keys pressed) and the
\ order in which they were pressed by presenting:
\ - an add/remove interface to handle key press/release
\ - "a current notes" or "last note" accessor
\
\ This allows proper handling of note to be played on a monosynth,
\ arpeggios on a monosynth and chords on a polysynth.

\ The data structure to do this is an ordered set.

\ A way to implement an ordered set of limited size (only 128 notes)
\ is to use an array where each element points to the next element or
\ an end-of-list marker.

\ Since we're storing MIDI notes, using a number from the #x80-#xFF
\ range seems obvious.  Let's pick #xFF as an init value and use the
\ sign bit to distinguish content from empty.

\ To make lookup faster, maintain invariant to keep all values set to
\ #xFF if they are not in the list to avoid having to chase the list
\ until a marker is found.

staapl pic18/arr
staapl pic18/cond
   
\ Instantiate circular buffer.
macro
: st-spec  #x180 5 ;  \ 2^5 = 32 byte
forth
: st-box   st-spec arr-box ;        \ index --  | a=box
: st-!     st-box !a  ;             \ el index --
: st-@     st-box @a  ;             \ index -- el

variable st-first
: st-init  st-mark dup st-first ! st-spec arr-fill ;  \ --


    
macro
: st-mark #xFF ;
: st-mark? st-mark = ; \ el -?
: st[ st-first @ begin dup st-mark? if drop exit then dup ;
: ]st st-@ again ;
forth

: _st-print
    st-first @
    begin
        dup st-mark? if drop ; then
        dup px st-@
    again

: st-print st[ px ]st    
  
: st-add \ el --
    dup >r st-first @ r> st-! \ link new head to old head
    st-first ! ;              \ store new head

\ To remove, find the element

: st-remove drop ;
    
\ variable st-last    
\ : st-remove \ el --
\     dup >r st-@ #xFF = if rdrop ; then  \ nop if not exist
\     st-first @
\     begin
\         \ ...
\     again
        
    

\ Keep track of the keys pressed and the order they are pressed in.
\ This can be used for:
\
\   - Tracking the youngest note: makes a monisynth more playable
\
\   - Keeping track of the arpeggiator sequence


\ The data structure is an array of note values, ordered according to
\ increasing event time.

variable nb-notes
: init-notes   0 nb-notes ! ;
: a!notes      0    
: a!notes+     al ! 2 ah ! ;  \ 256 byte buffer at #x200
: a!notes-endx nb-notes @ a!notes+ ;
macro    
: 1a-!         1 al -! ;
: 1a+!         1 al +! ;
forth
    
\ The operations on this data structure consist of:
\    
\   notes-add    append a note to the array if it's not there yet
\   notes-remove remove a note from the array if it's there
\   notes-last   retreive last note, or #xFF if there isn't any   


\ This is one of those examples where writing Forth code requires a
\ lot of thinking.  It took me a long time to get the original code
\ working.  I overlooked simple mistakes in different cases using
\ 'print debugging'.

\ The algorithm for notes-add is straightforward.  The notes-remove
\ operation is split into two parts: notes-index looks up the index of
\ the note and returns #xFF if it's not found.  notes-pop will remove
\ the element at a specified index by shifting all following events
\ one index down.



load debug.f
: print-notes a!notes nb-notes @ nz? if for a> px next else drop then ;
: fill-notes 1 5 for dup notes-add 1 + next drop ;    
: print-stacks  FSR0L @ px FSR1L @ px #x0A emit ;

    
: notes-last \ -- note
    nb-notes @ 0 = if #xFF ; then
    a!notes-endx 1a-! a> ;
    
: notes-add \ note --
    dup notes-index #xFF = if
        a!notes-endx >a
        1 nb-notes +!
    else
        drop
    then ;

: 2drop drop drop ;    
: notes-index \ note -- i
    a!notes nb-notes @
    z? if 2drop #xFF ; then
    for
        a> =? if
            2drop
            al @ 1 -
            a!notes al @ -
            r> drop  \ FIXME: this is bad style!!!  looking for half an hour for this missing line..
            ;
        then drop
    next
    drop #xFF ;
    
: notes-pop \ index --
    dup 1 + a!notes+       \ point past first element to remove
    nb-notes @ - for
        @a- dup >a >a
    next
    1 nb-notes -! ;

: notes-remove \ note --
    notes-index
    dup #xFF = if drop ; then
    notes-pop ;
    
    



    
\ \ Yes cute, but actually less code and faster if array is not too large.
\ macro    
\ : _notes-pop \ index --
\     1 nb-notes -!
\     3 min << \ each slot contains a movff instruction of 2 words wide
\     route
\         n1 n0 @!
\         n2 n1 @!
\         n3 n2 @! ;
\ forth
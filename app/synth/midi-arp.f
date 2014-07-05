\ Keep track of which notes are on.  Play last one.

variable nb-notes
: init-notes 0 nb-notes ! ;
: a!notes    0 3 a!! ;

\ load debug.f
\ : print-notes a!notes nb-notes @ nz? if for a> px next else drop then ;
\ : fill-notes 1 5 for dup notes-add 1 + next drop ;    

: notes-last \ -- note
    nb-notes @ z? if drop #xFF ; then
    a!notes 1 - al +! a> ;
    
: notes-add \ note --
    dup notes-index #xFF = if
        a!notes
        nb-notes @ al +! >a
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
    a!notes dup 1 + al +!  \ point past element to remove
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
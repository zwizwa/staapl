\ maybe useful?

variable nb-notes
variable n0
variable n1
variable n2
variable n3

\ load debug.f
\ : print-notes a!notes 1 al -! nb-notes @ 1 + for a> px next ;
\ : fill-notes 1 5 for dup notes-add 1 + next drop ;    

\ seems to work.
\ todo: find current note  
  
: init-notes 0 nb-notes !
: a!notes n0 0 a!! ;    

    
: notes-add \ note --
    a!notes
    nb-notes @ al +! >a
    1 nb-notes +! ;

: ~notes-index \ note -- i
    a!notes
    nb-notes @ nz? if
        for
            a> =? if
                drop drop al @ 1 - n0 - ;
            then drop
        next
    then drop xdrop ;  \ abort calling word
    
: ~notes-pop \ index --
    a!notes dup 1 + al +!  \ point past element to remove
    nb-notes @ - for
        @a- dup >a >a
    next
    1 nb-notes -! ;

: notes-remove \ note --
    ~notes-index \ aborts this word if not found
    ~notes-pop ;
    
    



    
\ Yes cute, but actually less code and faster if array is not too large.
macro    
: _notes-pop \ index --
    1 nb-notes -!
    3 min << \ each slot contains a movff instruction of 2 words wide
    route
        n1 n0 @!
        n2 n1 @!
        n3 n2 @! ;
forth
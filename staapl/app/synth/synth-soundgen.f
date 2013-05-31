
\ the nicest paradigm for controlling sounds is that of virtual
\ samples: splitting up the sound generation in two tasks gives a more
\ natural way of describing sound:
\
\  - virtual samples: infinite loops that describe the evolution of a
\    sound, using the sync mechanisms available.
\
\  - trigger controller: a task that can restart different virtual
\    samples, also using the sync mechanisms available.
\
\ the basic idea is to separate MODULATION from EVENTS.



\ We're only using 2 tasks, so just need one task variable to save the
\ suspended tasks' state pointer.

variable other-task  

: yield
    suspend
    other-task swaptask
    resume ;

\ The task switch point is the synchronization wait loop. This is the
\ same as the sync-tick macro from synth-control.f but with yielding
\ inserted in the loop.

macro    
: sync | bit | \ --
    begin yield bit tickbit low?  until
    begin yield bit tickbit high? until ;
forth    


\ Memory allocation for the virtual sample player task.

macro
: vsamp-rs #x10 ;  \ half of the hardware stack
: vsamp-ds #xE0 ;  \ DS and XS at top of memory (shadowed by SFRs)
: vsamp-xs #xF0 ;   
forth  
  

\ Booting a new virtual sample task. This basically executes a word in
\ a newly created context. This uses byte addressing for the task.

\ The current vsamp is stored in a vector, so it can be
\ retriggered. Just set it using the '->' word.
  
2variable sound

: bang \ --
    suspend other-task !  \ suspend the controller task
    vsamp-rs rp !         \ set the pointers
    vsamp-xs xp !
    vsamp-ds dp !
    sound invoke ;


\ utility
macro
: tail begin yield again ;
forth
    
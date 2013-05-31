\ vm memory model.

\ there are a lot of design decisions here. one is already made above:
\ code cannot reside in ram. maybe that's a bad idea? another one is:
\ should STORE write flash memory? terribly convenient, but highly
\ inefficient.
	
\ since the vm has an effective word address range of 32kb, i'm going
\ to use the upper half for flash, and the lower half for ram. eeprom
\ could be added somewhere when necessary. 

macro
: ram? 1st 7 low? ;
forth

\ FETCH  

: flash-enter
    fh @ >x fl @ >x   \ save f
    fl ! fh ! ;

: flash-leave    
    x> fl ! x> fh @ ; \ restore f

: _@
    ram? if a!! @a+ @a+ ; then
    #x7F and

: _@_flash
    flash-enter
    @f+ @f+
    flash-leave 


: _@c
    ram? if a!! @a+ 0 ; then
: _@c_flash    
    flash-enter
    @f+ 0
    flash-leave ;

\ STORE

\ only mapped to ram.
: _!   a!! swap>x !a+ x> !a+ ;
: _!c  a!! drop !a+ ;

    

    

    
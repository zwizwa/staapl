\ DTC task switching


\ Save/restore data stack pointer.
: dp@
    dup dup
    movff FSR0L WREG
    movff FSR0H INDF0 ;
: dp!
    INDF0 FSR0H movff
    WREG  FSR0L movff
    drop drop ;


\ Switch data stack, copying message.
\ n lo hi --    


: dp@
    dup \ FSR0 now points to whole stack
    movff FSR0H POSTDEC0
    movff FSR0L WREG ;


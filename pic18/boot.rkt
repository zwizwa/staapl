#lang staapl/pic18  \ -*- forth -*-
provide-all

\ Boot init macros for PIC18 chips.
macro
  
\ stack init code
: init-xs 0 movlw STKPTR movwf ; \ XS (hardware return stack / eXecute stack)
: init-ds 1 - 0 lfsr ;  \ DS
: init-rs 1 - 1 lfsr ;  \ RS (retain stack)

\ * INDF0 the data stack pointer
\ * INDF1 the byte-sized retain stack (r) pointer
\ * STKPTR the hardware return stack (x) pointer

\ boot

\ The PIC18 has 64-byte flash erase blocks.  The boot and ISR vectors
\ all reside in block 0, therefore we don't put any library code here,
\ and reserve the block for vectors only.  If block 0 is cleared it
\ contains 32 #xFFFF NOP instructions, which means execution will fall
\ through to block 1 on reset.  We use block 1 as the "debugger boot block".

    
: boot-vector! | warm |
    ` block1 >label | block1 |

    #x0000 org-begin
        block1 jw      \ jump over ISR vectors if any
    org-end
    
    #x0040 org-begin
        block1 enter:  \ tag label for previous jump
        warm i exit    \ jump to setup code
    org-end
;

: init-isr | action address |
    address org-begin action i exit org-end ;
    
: init-isr-hi #x0008 init-isr ;
: init-isr-lo #x0018 init-isr ;
    
\ For lo-pri ISR we need to save registers manually.  The tricky part
\ here is STATUS because the ordinary stack op "drop" uses MOVF, which
\ affects the flags.  The code below is used "STATUS !" to make sure
\ MOVF is not used, but MOVFF is used instead (nfdrop).
: STATUS@ STATUS @ ;
: STATUS! dup STATUS ! nfdrop ;    
    
\ fosc
: kHz  1000 * ;
: MHz  kHz kHz ;



    

\ Misc  stuff to remember when configuring chip

\ - disable watchdog timer if the code does not use CLWDT
\ - set power up timer (or use capacitor + reset enabled)
\ - disable MCLR pin, or use a reset switch with pullup resistor

\ Data memory

\ Note that the extended 18f instruction set allows the use of indexed
\ addressing relative to FSR2, which can enable cheap structs/objects. 
\ however, it disables global (access bank) variables for the region
\ 0x000-0x05F, which makes that region better suited for stack memory
\ if the extensions are used, so the contiguous space after 0x80 could
\ be used as object memory, leaving the space 0x60-7F for global
\ variables.

\ in short, indexed addressing?
\ no  -> stacks in 0x80 - 0xFF, globals in 0x00 - 0x7F
\ yes -> stacks in 0x00 - 0x5F, globals in 0x60 - 0x7F

\ Note that bank selecting is not used. it sucks, one can do without

forth

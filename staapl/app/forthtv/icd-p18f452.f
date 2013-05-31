
\ ICD serial example for PIC18F452 @ 40Mhz

\ Chip config.  Look in the data sheet for these.
#x300000 org
  #x00 , #x26 , #x0F , #x0E ,  
  #x00 , #x01 , #x81 , #x00 ,
  #x0F , #xC0 , #x0F , #xE0 ,   \ no write protect
  #x0F , #x40 , 

load p18f452.f           \ chip macros
load monitor-icd-core.f  \ boot block + serial monitor code

load forthtv.f
load ball.f
load logo.f
\ load hook.f

: warm init-all interpreter ;

allot-flash org
    
\ (re)define some macros.  Note that because macro (re)definition
\ always happens before code generation, macros defined at the end of
\ the source code will affect all the code generation.

macro
: rx-ready? icd.rx-ready? ;
  
: fosc 40 MHz ;     \ 4 clock cycles per instruction cycle gives 10 MIPS.
: baud 19200 ;      \ Bit-banged.  38400 didn't work..
: boot-size #x500 ; \ 64 byte aligned, needs to fit boot image.
forth


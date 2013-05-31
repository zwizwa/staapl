
\ ICD serial example for PIC18F2620 @ 40Mhz

\ Chip config.  Look in the data sheet for these.
#x300000 org
  #x00 , #x26 , #x0F , #x0E ,  
  #x00 , #x01 , #x81 , #x00 ,
  #x0F , #xC0 , #x0F , #xE0 ,  \ B: boot write protect
  #x0F , #x40 , 

load p18f2620.f     \ chip macros
load monitor-icd.f  \ boot block + serial monitor code

\ (re)define some macros.  Note that because macro (re)definition
\ always happens before code generation, macros defined at the end of
\ the source code will affect all the code generation.

macro
: fosc 40 MHz ;     \ 4 clock cycles per instruction cycle gives 10 MIPS.
: baud 9600 ;       \ Bit-banged, keep it low.
: boot-size #x300 ; \ Not using boot write protect, so make smaller.
forth


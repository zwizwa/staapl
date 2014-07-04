\ -*- forth -*-

\ This is an example Forth file for the 18f452 chip with serial
\ connection.


\ config bits
#x300000 org

  #x00 , #x26 , #x0F , #x0E ,  
  #x00 , #x01 , #x81 , #x00 ,
  #x0F , #xC0 , #x0F , #xA0 ,  \ B: boot write protect
  #x0F , #x40 , 


macro

\ time
: fosc 40 MHz ;
: baud 38400 ;

forth
  
load p18f452.f          \ chip macros
load monitor-serial.f   \ boot: shared serial monitor/interpeter code

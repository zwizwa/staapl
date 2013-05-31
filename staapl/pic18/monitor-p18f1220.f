\ -*- forth -*-

\ This is an example Forth file for the 18f1220 chip with serial
\ connection.

\ config bits
\ internal oscillator, watchdog disabled
#x300000 org
 #x00 , #xC8 , #x0F , #x00 ,  \ 0 1 2 3
 #x00 , #x80 , #x80 , #x00 ,  \ 4 5 6 7
 #x03 , #xC0 , #x03 , #xA0 ,  \ 8 9 A B  \ B : boot protect on / off = #xA0 / #xE0
 #x03 , #x40 ,                \ C D



\ time
: fosc 8000000 ;
: baud 9600 ;

forth

load p18f1220.f         \ chip macros
load monitor-serial.f   \ boot: shared serial monitor/interpeter code


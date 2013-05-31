#lang scheme/base
(require "pk2.ss")
(pk2-boot)
(pk2-program
 '(0 1 2 3 4 5 6 7)
 '(#x21  #x02  #x3A  #x1E #x00  #x81  #x85  #x00 #x0F  #xC0  #x0F  #xE0 #x0F  #x40)
 'PIC18F2550)

         
         

#lang staapl/pic18 \ -*- forth -*-
provide-all
staapl pic18/vector

2variable stdout
: >o stdout invoke ;

\ These are small and often used.
: a>o  stdout -> !a+ ;
: f>o  stdout -> !f+ ;    
: d>o  stdout -> ;        \ data stack
 

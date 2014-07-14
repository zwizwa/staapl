#lang staapl/pic18 \ -*- forth -*-
provide-all
staapl pic18/vector

2variable stdout       : >o       stdout       invoke ;
2variable stdout-flush : o-flush  stdout-flush invoke ;
    
\ These are small and often used.
: o=a  stdout -> !a+ ;
: o=f  stdout -> !f+ ;    
: o=d  stdout -> ;        \ data stack
 

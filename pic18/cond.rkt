#lang staapl/pic18 \ -*- forth -*-
provide-all


\ Standard forth conditionsals.  These drop the input arguments as
\ opposed to the words ending in ? which leave the arguments intact.

macro    
: >= - nfdrop c? ; \ a b -- ?    | >=? \ a b -- a b ?
: =  - nfdrop z? ; \ a b -- ?    | =?  \ a b -- a b ?
forth     

\ Note: these are all macros because `if' does not yet support
\ run-time conditions as numbers on the stack.


  
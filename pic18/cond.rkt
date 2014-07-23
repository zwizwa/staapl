#lang staapl/pic18 \ -*- forth -*-
provide-all


\ Standard forth conditionals.  These drop the input arguments as
\ opposed to the words ending in ? which leave the arguments intact.

macro    
: >= - nfdrop c? ; \ a b -- ?    | >=? \ a b -- a b ?
: =  - nfdrop z? ; \ a b -- ?    | =?  \ a b -- a b ?
: <  >= not ;
: 0=? 0 addlw z? ; \ a -- a ?    | (1)
forth     

\ (1) Kernel code uses a lot of prematurly optimized z? words.  This
\ is works for lowlevel (macro) arithmetic, logic and loads to top of
\ stack.  However it's not always clear when writing highlevel code
\ whether the last machine word set the flags.  In general it's best
\ to use 0=? instead of z?

  
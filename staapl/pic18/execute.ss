#lang staapl/pic18 \ -*- forth -*-

provide-all

\ For PIC18, execute doesn't make much sense: 8 bit address space
\ doesn't bring you very far. However, the 16 bit version does make
\ sense.

\ So let's define an execution token as a 2 byte value. Does this lead
\ to trouble?

\ Note that the value of a symbol representing a code address always
\ uses WORD ADDRESSING unless otherwise indicated. This is different
\ from table reading and writing, which uses BYTE ADDRESSING. I found
\ this to be the sanest way of dealing with addresses. It's a
\ trade--off slightly in favour if this solution.

\ As a result, _execute does need to shift its arguments.

macro
: lohi dup #xFF and swap 8 >>> ;
forth  

: execute/w 2nd rot<<c! rot<<c     \ the LSB is ignored by the machine..
: execute/b push TOSH ! TOSL ! ;   \ ( lo hi -- ) execute STC primitive, byte address

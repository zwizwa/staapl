#lang staapl/pic18  \ -*- forth -*-
provide-all

: _route/e              \ clip to last
    min
: _route 
    rot<< dup TOSL +!	\ can use rot since LSB is ignored..
    1 and TOSH ++! ;	\ ..which enables us to use it here

macro
: route
    _route     \ proper call
    end: ;     \ avoids tail call if route is followed by . or ;
: route/e
    _route/e
    end: ;
forth
  

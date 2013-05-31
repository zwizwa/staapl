\ conditional
: _choose
    _>r _>r
    or z? if drop   _rdrop _r> ;
        then drop   _r> _rdrop ;

\ quoted program
: _quot  2 fl @ +
         0 fh @ ++ ;
    


  
\ quotation.. the construct { A B C } has the runtime behaviour:
\ 1. load address of the composite function
\ 2. jump over the code

\ jumping can be eliminated by compiling the composite code after the
\ containing word, however, it's probably easier to juse use a parsing
\ word that compiles a jump over the quoted code for simple on-target
\ compilation.


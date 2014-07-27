macro
\ : lohi | x | x x 8 >>> ;  
: idtc i/c cw>label 2 * lohi execute/dtc ;
forth

: test1 [
    enter
    #x1234 _literal
    ' _dup _compile
    ' _exit _compile
    ] idtc ;

: ~test2
    enter
    _if
    1 _literal
    _then
    ' _exit _compile
: test2 ' ~test2 idtc ; \ works for nonzero argument, not for zero

: ~test3
    enter
    _if
    1 _literal
    ' _exit _compile
    _then
    2 _literal
    ' _exit _compile
: test3 ' ~test3 idtc ; \ doesnt work

: ~test4 enter _begin _again   
: test4 ' ~test4 idtc ;

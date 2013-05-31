staapl pic18/execute

2variable i-vec
2variable o-vec

: i> i-vec @ i-vec 1 + @ execute/b ;
: >o o-vec @ o-vec 1 + @ execute/b ;    

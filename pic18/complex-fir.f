load mac-u8xu8.f

\ The macros above are used to instantiate a mac routine using a
\ global complex 2x24 bit accumulator.

2variable acc-i variable hr-i  \ inphase    (real)
2variable acc-q variable hr-q  \ quadrature (imaginary)

\ An extra accumulator, used for computing offset correction terms due
\ to unsigned multiplication.

2variable acc-dc

\ needs to be independent of 'a' register, and fast
: clear-acc
    acc-i  3clear
    acc-q  3clear
    acc-dc 2clear ;

\ The filter loop, reads 8 bit unsigned coefficiens from ram (real)
\ and flash (complex), using the 'a' and 'f' registers.

\ n --
: fir-loop
    for
        @a+  \ input data stored in ram
        dup acc-i  mac-u8xu8-24  \ real
        dup acc-q  mac-u8xu8-24  \ imaginary
            acc-dc mac-dc-16     \ average (for DC compensation)
    next ;


: fir-correct
    acc-i acc-dc acc-correct
    acc-q acc-dc acc-correct ;


\ Main filter routine. This requires 'a' and 'f' registers to be set
\ to the unsigned signal and #x80-centered unsigned filter
\ coefficients.

\ n --
    
: fir
    clear-acc
    fir-loop
    fir-correct ;

macro
: u, #x80 + , ;  
forth  
    
\ Test routines
\ : average f->
\     1 u,  1 u,
\     2 u, -1 u,
\     2 u,  1 u,
\     1 u, -1 u,

\ : input
\     #x40 0 a!! ;


\ test can be:  
\    1 #x80 + init-test
\    input average 4 fir    


\ : n!a
\     for dup !a+ next drop ;

    
\ : init-test \ value --
\     input #x40 n!a  \ set buffer to all 1s, dc shifted
\     ;



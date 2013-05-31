\ 16bit multiplication (global temp)

variable R0
variable R1
variable R2
variable R3

variable X0
variable X1
variable Y0
variable Y1

  
\ load the core multiplier routine which binds to the global temp
\ variables above
  
load mul16.f

: YX!  Y! X! ;
: R@   RL@ RH@ ;
  
: _umul  YX! umul>R R@ ;   \ unsigned, double
: _smul  YX! smul>R R@ ;   \ signed, double

: _fumul YX! umul>R RH@ ;  \ unsigned, fixed

    
: _*  \ signed, single word
    YX! smul>R
    RL@ ;  

: _*s \ signed, single word, saturated
    YX! smul>R
    R-saturate-signed ;

\ signed fractional. instead of keeping 1:0, we keep 2:1. same logic,
\ different registers.
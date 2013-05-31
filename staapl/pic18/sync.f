\ input waits for 1->0 start bit transition, waits half a bit, then
\ clocks in bits, including the stop bit.

macro
: wait | q-cond? |
    q-cond? i if begin q-cond? i not until then
                 begin q-cond? i     until ;
             forth
               
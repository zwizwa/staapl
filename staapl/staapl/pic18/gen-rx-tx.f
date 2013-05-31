\ Generic interface for byte receiver-transmitter systems.

\ pc is short for "prefix compile" wich combines two symbols into a
\ name which is then compiled. This is the most minimal name space
\ management that is still useful to create some form of generic
\ algorithms.

macro
: rx-ready?  ' rx-ready? pc ;  \ class -- ?
: tx-ready?  ' tx-ready? pc ;  \ class -- ?
: >tx        ' >tx pc ;        \ byte class --
: rx>        ' rx> pc ;        \ class -- byte
: >tx        ' >tx pc ;        \ byte class --
: rx>        ' rx> pc ;        \ class -- byte
forth

  
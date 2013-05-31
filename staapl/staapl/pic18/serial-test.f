
staapl pic18/serial

: serial-test
    230400 40000000 init-serial
    0 begin dup async.>tx 1 + again
    
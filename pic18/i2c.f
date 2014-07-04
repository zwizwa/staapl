\ I2C serial bus driver

\ for 18f2620
macro

\ these need to be configured as in or out  
: i2c-clock TRISC 3 ;
: i2c-data  TRISC 4 ;
: output low ;
: input  high ;    
: master output ;
: slave  input ;  


    
: ic2-rx-ready? SSPSTAT

\ In slave mode both clock and data need to be input: the MSSP will
\ override to output state during transmission. In slave mode there is
\ always an interrupt on address match. It's possible to get
\ interrupts on start/stop bits too.

: init-ic2-master
    ic2-clock 
    #b00101000 SSPCON1 ! \ enable + master mode: clock = FOSC/(4 * (SSPADD + 1))

;

forth
  
\ blocking receive    
: ic2-receive
    begin ic2-rx-ready? until
    SSPBUF @ ;
    
\ SSPBUF = serial data I/O register    
\ SSPADD = device address
\ SSPCON1, SSPCON2, SSPSTAT = control registers  


\ Since I2C is a shared bus structure, it's easiest to interface to it
\ using a multi tasking / message passing approach. There are 2 levels:

\ - DRIVER: reads from message buffer, writes out to bus when ready
\ writes to message buffer, and wakes up tasks.

\ - TASK: a task maps a single message to zero or more output
\ messages. message sending never blocks.

    
    
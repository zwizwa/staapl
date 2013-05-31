\ Message buffering for a shared bus architecture. The topology looks
\ like this:

\                            wire
\                              |
\                              | G
\         A             E      v      F
\  wire ----> [ LRX ] ----> [ LTX ] ----> wire
\                |             ^
\  . . . . . . . | B . . . . . | D . . . . . . .
\                v      C      |
\             [ HRX ] ----> [ HTX ]
\
\ Code above the dotted line runs with interrupts disabled, and
\ pre--empts the code below the line. Communication between the two
\ priority levels uses single reader - single writer buffers. The 6
\ different events are:
\
\ A) PIC hardware interrupt
\ B) RX buffer full condition
\ C) TX buffer full condition (execute task which writes to buffer)
\ D) wakeup lowlevel TX task from userspace
\ E) wakeup lowlevel TX task from kernelspace
\ F) PIC hardware send
\ G) wakeup lowlevel TX task from bus idle event
\
\ A task is an 'event converter'. The 4 different tasks are:
\
\ LRX) convert interrupt (A) to tx buffer full B and tx wakeup E
\ HRX) convert tx buffer full (B) to rx buffer full (C)
\ HTX) convert tx buffer full (C) to tx wakeup (D)
\ LTX) convert wakeup (data ready: D,E) to hardware send.
\
\ The pre--emption point is A: this causes no problems for the
\ low--priority task because of the decoupling provided by the receive
\ buffer. The only point that needs special attention is the LTX task,
\ which can be woken up by different events D, E and G, and care needs
\ to be taken to properly serialize message handling. To do this, both
\ D and E should invoke LTX with interrupts disabled. For E this is
\ trivial: just call the LTX task, for G is is already ok since it's
\ an isr, so D needs to explicitly disable interrupts.
\



\ uses the following generic operations
\
\     ready?      \ -- ?
\     read        \ -- byte
\     write       \ byte --
\

macro

: or; not if exit then ;

\ transfer bytes from one object to another + exit current word when
\ done. the 'exit' bit is a poor man's break: it's easy to escape from
\ a nested control structure using the return stack, since compilation
\ stack and return stack are independent (but serve somehow the same
\ purpose).
    
: transfer; src dst | \ --
    
    begin
	' ready? src msg or;
	' ready? dst msg or;
	' read   src msg
	' write  dst msg
    again

    ;
forth

 



\ macro
\ : rx-message ? 0 0 high? ; \ complete highlevel message received  

\ forth  


\ : lowlevel-rx
\     buffer-rx-byte  \ read byte from device
\     begin tx-message? while
\ 	    dispatch-message
\     repeat
\     highlevel-tx ;  \ check if there are remaining messages to send

\ : lowlevel-tx
\     begin tx-message? while
\     repeat
    

\ \ After full reception of a message, the handling code is called. This
\ \ might do more dispatching based on the contents of the message.

\ : dispatch-message ;



    
\ : highlevel-tx ;   



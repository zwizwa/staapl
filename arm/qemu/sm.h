#ifndef SM_H
#define SM_H

#include <stdint.h>

/* Composable State Machines in C. */

/* LICENSE:

   This code (a single C header file) is placed in the public
   domain by its author, Tom Schouten.

   The code is based on original research on embedded software
   patterns in C and other languages, performed throughout the period
   2002 - 2015 */


/* RATIONALE:

   Most state machines encountered "in the wild" in embedded software
   systems are linear sequences of code, possibly with some form of
   early abort in case of errors.

   In the presence of an RTOS, these would be most conveniently
   expressed as blocking tasks, possibly with nested subroutines.

   However, when an RTOS is not available or appropriate, the
   abstraction in this file can lift some of the nuisance associated
   with implementing state machines manually (E.g. using explicit
   switch statements and/or function pointers).

   It implements abstractions for:

   - Waiting on a condition to be satisfied (SM_WAIT).

   - Code reuse for re-occuring subtasks (SM_CALL).


   The abstractions allow operation both from a main event loop
   polling routine calling a main _tick() method, and a ISR, e.g. to
   _tick() only when an event is there.

   A note on SM_CALL: Instead of using a call stack (as is done in the
   RTOS task case), the caller of a subroutine (submachine) manages
   its callee state explicitly as a constituent of its own state.

*/


/* IMPLEMENTATION:

   The implementation uses some GCC extensions:

   - computed goto / GCC labels as values.
     https://gcc.gnu.org/onlinedocs/gcc/Labels-as-Values.html

   - the __COUNTER__ macro to implement GENSYM.
     https://gcc.gnu.org/onlinedocs/cpp/Common-Predefined-Macros.html

   - statement expressions:
     https://gcc.gnu.org/onlinedocs/gcc-3.1/gcc/Statement-Exprs.html

   At the expense of convenience, it is possible to use switch()
   statements and manual label generation to translate this to
   portable code.



   A state machine consists of:

   - a struct with a "next" member to contain the C code resume point
   - a _init() method to initialize the struct
   - a _tick() method to perform the next action, containing
   - a "halt" label (pointing to SM_HALT) if SM_WAIT_TICK is used

   These _tick() methods have a body that looks like:

   {
     SM_RESUME(sm);
     // ... C code with SM_WAIT(sm, ...) statements
   halt:
     SM_HALT(sm);
   }

   SM_RESUME() enters the state machine, executing the following C
   code statement if the machine was resently initialized, or resuming
   where it left off in a previous call to _tick().

   An SM_WAIT() statement will evaluate a condition and if it is
   false, it will exit the _tick() method after puttimg the machine in
   a state such that the condition is re-evaluated the next time
   _tick() is executed.  In this case _tick() returns SM_WAITING.

   SM_HALT() loops forever in halting state, signalling this condition
   by the SM_HALTED return value.


   Caveat: Take specoal care in using local/automatic variables inside
   a _tick() function.  These are only valid inbetween suspension
   points.  However, GCC should produce warnings of uninitialized
   variables.

*/

#ifndef CONCAT
/* Identifier concatenation. */
#define _CONCAT(X,Y)  X##Y
#define CONCAT(X,Y)   _CONCAT(X,Y)
#endif

/* Per-file original symbol generation */
#ifndef GENSYM
#define GENSYM(sym)   CONCAT(sym,__COUNTER__)
#endif


/* Return codes.  Other values are used to signal errors. */
#define SM_HALTED     0
#define SM_READY      0xFFFFFFFEUL  // yield point; caller can fetch output.
#define SM_WAITING    0xFFFFFFFFUL

/* sm->next contains the address of the C code resume point. */
#define SM_RESUME(sm) do {                      \
    if (sm->next) goto *(sm->next) ; } while(0)
#define _SM_WAIT(sm,label,condition) do {       \
    label:                                      \
      if (!(condition)) {                       \
          sm->next = &&label;                   \
          return SM_WAITING;                    \
      }} while (0)
#define SM_WAIT(sm,condition)                   \
    _SM_WAIT(sm,GENSYM(label_),condition)
#define _SM_HALT(sm, halt_loop)                 \
  do {                                          \
  halt_loop:                                    \
    sm->next = &&halt_loop;                     \
    return SM_HALTED; } while(0)
#define SM_HALT(sm)                             \
    _SM_HALT(sm,GENSYM(label_))

/* Return suspend. */
#define _SM_YIELD(sm,label) do {                \
        sm->next = &&label;                     \
        return SM_READY;                        \
      label:                                    \
        if (0);                                 \
    } while (0)

#define SM_YIELD(sm)                            \
    _SM_YIELD(sm,GENSYM(label_))


/* Running sub-machines.

   A sub-machine is the state machine analogue of a sub-routine: it is
   another state machine that is initialized by its caller, "ticked"
   until it halts, upon which control is transferred back to its
   caller.

   The code below uses the return values of the _tick() method:

   - SM_WAITING  no error, waiting for condition (needs another tick())
   - SM_HALTED   no error, done running.  Resume execution of caller.
   - other       error code: propagate error

*/

/* Optionally abort on error. */
#define _SM_WAIT_TICK(sm,label,tick,abort) ({   \
        uint32_t rv;                            \
      label:                                    \
        rv = tick;                              \
        switch(rv) {                            \
        case SM_HALTED:                         \
            break;                              \
        case SM_WAITING:                        \
            sm->next = &&label;                 \
            return rv;                          \
        default:                                \
            if (abort) {                        \
                sm->next = &&halt;              \
                return rv;                      \
            }                                   \
            else break;                         \
        }                                       \
        rv;})

#define SM_WAIT_TICK(sm,tick,abort)             \
    _SM_WAIT_TICK(sm,GENSYM(label_),tick,abort)

/* Using SM_WAIT_TICK, provide a convenience routine that initializes
   a state machine with standard naming convention and runs it to
   completion or error. */
#define SM_CALL(sm,name,state,...) do {                 \
    name##_init(state, __VA_ARGS__);                    \
    SM_WAIT_TICK(sm,name##_tick(state),1); } while(0)

/* Same, but don't abort on error, returning error value instead. */
#define SM_CALL_CATCH(sm,name,state,...) ({     \
    name##_init(state, __VA_ARGS__);            \
    SM_WAIT_TICK(sm,name##_tick(state),0); })

/* Run with busy-wait (for testing) */
#define SM_RUN_BUSYWAIT(sm, name, ...) ({               \
    name##_init(sm, __VA_ARGS__);                       \
    uint32_t rv;                                        \
    while (SM_WAITING == (rv = name##_tick(sm)));       \
    rv;})


/* The above is enough to implement a small "operating system".  Here
   we add some buffering abstractions. */


/* Buffer abstraction. */
struct sm_buf_u16 {
    uint16_t *next;
    uint16_t *endx;
};
// same for u8, u32, ...

/* Blocking write to buffer.  Write as long as there is room.

   Note that it is more efficient to iterate over the buffer on the
   writing side than to yield on every element. */

#define SM_WAIT_BUF_WRITE(sm, smb, typ, data) do {    \
        SM_WAIT(sm, (smb)->next < (smb)->endx);       \
        *((smb)->next)++ = data; } while(0)

#endif

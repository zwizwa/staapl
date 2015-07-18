#include "sm.h"

/* Staapl bootstrap kernel in C, on top of sm.h API to incorporate in
   bare-bones C firmware.

   Due to the depth of the recursion, a previous attempt at writing an
   interpreter required a lot of macros to be able to expand the
   suspend points into the main _tick() function.

   It seems better to add a return stack to the state machine.  Doing
   so it's probably best to go all the way and interpret a threaded
   Forth kernel in C.

   Goals are fluid here..  The main goal is compact code on ARM Cortex
   M, with possible native code as well.  DTC might be best to start.

*/

#include <stdlib.h>
#include <setjmp.h>

/* Unpacking Return Stack (URS) Forth machine.

   - Main design property: buffered bytecode execution with chunked
     instruction transfer.

   - Code storage is abstract.

*/


/* Generic command parser and backdoor. */

/* Design:
   - should incrementally read byte stream == unaligned
   - as flexible as possible without firmware upgrade
   - trade off flexibility (call any C function) with genrality (some
     functions are "blessed" in an interface.
   - if possible, also implement stream router
*/

enum urs_opcode {
    URS_LIT = 0, // -- n     // read one byte from input stream
    URS_CPY = 1, // n -- ... // multiple bytes from input to param stack
    URS_EXC = 2, // arg1 ... argn code n -- // execute C code
};

enum urs_err {
    URS_ERR_RUNNING = 0,
    URS_ERR_EXIT = 1,     // Normal exit.
};

struct urs_parser;
typedef void (*urs_state_t)(struct urs_parser *);

#define URS_DS_SIZE 64
struct urs_parser {
    urs_state_t state;             // input parser state
    cyg_uint8 ps[URS_DS_SIZE];     // parameter stack
    int psp;                       // parameter stack pointer
    jmp_buf env;
};

void urs_exit(struct urs_parser *p) {
    longjmp(p->env, URS_ERR_EXIT);
}

void urs_read_opc(struct urs_parser *p);
void urs_read_lit(struct urs_parser *p);

void urs_read_lit(struct urs_parser *p) {
}

enum urs_err urs_next(struct urs_parser *p) {
    int rv;
    /* Loop until longjmp() is called. */
    if ((rv = setjmp(p->env))) return rv;
    else { for(;;) p->state(p); }
}

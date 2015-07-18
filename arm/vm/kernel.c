
/* Staapl bootstrap kernel in C, written as non-blocking _tick() to
   incorporate in bare-bones C firmware.

   This uses an "ARMified" C representation, where the first 4
   arguments are kept in registers.

   Goals are fluid here..  The main goal is compact code on ARM Cortex
   M, with possible native code as well.  DTC might be best to start.

*/

// FIXME: still trying to find right calling convention.  It would be
// nice to have R0-R3 passed in tail calls, but can't get there..
// Looks like ASM is needed.


#include <stdint.h>


/* Trick GCC into passing state in R0-R3. */
struct state {
    uint32_t ds;
    uint32_t rs;
    uint32_t ip;
    uint32_t xs;
};
//typedef struct state vstate __attribute__ ((__vector_size__ (16)));
typedef int vstate __attribute__ ((__vector_size__ (16)));



/* Protocol only uses an 8 bit interface, but the VM is 32bit. */
union word;
typedef uint32_t status;



#define REGS_PROTO  union word *ds, union word *rs, union word *ip, union word *xs
#define REGS        ds,rs,ip,xs
#define NR          //__attribute__((__noreturn__))
#define DEF(name)   NR void name(REGS_PROTO)
#define NEXT        next(REGS)
#define CALL(word)  word(REGS)
#define PUSH(stack) (*++(stack))
#define POP(stack)  (*(stack)--)

typedef void (code)(REGS_PROTO);
union word {
    union word* pw;
    uint32_t    u32;
    uint8_t     u8;
    code*       xt;
};

DEF(next) { (*ip++).xt(REGS); }

DEF(dup)  { PUSH(ds) = ds[0]; NEXT; }
DEF(drop) { POP(ds); NEXT; }

#define DUP CALL(dup)

struct vm {
    union word *ds;
    union word *rs;
    union word *ip;
    union word *xs;
};

void vm_init(struct vm *vm, union word *ds, union word *rs) {
    vm->ds = ds;
    vm->rs = rs;
}

vstate test(vstate s) {
    //s[0]++;
    return s;
}

#define VM(word) word(vm->ds, vm->rs, vm->ip, vm->xs)
uint32_t vm_tick(struct vm *vm) {
    VM(dup);
    VM(drop);

    vstate v = {0,0,0,0};
    test(v);
    return v[0];
}


struct vm vm;
union word ds[1024];
union word rs[1024];

void reset(void) {
    
    vm_init(&vm,&ds[-1],&rs[-1]);
    for(;;) vm_tick(&vm);
}

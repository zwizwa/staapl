#include <stdint.h>

// Bootstrap interpreter for Staapl on ARM.
// see:
// http://balau82.wordpress.com/2010/11/30/emulating-arm-pl011-serial-ports/
// 

#include "sm.h"

/* Log console.  This is assumed to be non-blocking, i.e. not
   requiring an SM suspend point. */
const uint8_t hexdigit[] = "0123456789ABCDEF";


/* SM version of Staapl interpreter */
#define SM_VM_STACK_BYTES 1024
struct sm_vm {
    void *next;
    uint32_t count;
    uint8_t stack[SM_VM_STACK_BYTES];
    uint8_t *sp;
    uint8_t *areg;
    uint8_t *freg;
    uint8_t *ireg;
    void *uart;
};

void sm_vm_init(struct sm_vm *sm) {
    sm->sp = &sm->stack[-1];
    sm->areg = 0;
    sm->freg = 0;
    sm->ireg = 0;
}

/* These are macros because they contain suspend points, which need to
   be flattened out into the containing _tick() function.

   Macros are "non-hygienic", e.g. defined in sm context.  Doesn't
   matter since they are not exported.

   It's possible to factor this out a bit into sub-machines.
*/

#define TX(c) ({ SM_WAIT(sm, uart_tx_ready(sm->uart)); infof("T:%02x", c); uart_tx(sm->uart, c); })
#define RX(c) ({ SM_WAIT(sm, uart_rx_ready(sm->uart)); uint8_t c = uart_rx(sm->uart); infof("R:%02x", c); c;})

#define REPLY(m) ({ TX(0xFF), TX(n); })
#define ACK()    ({ REPLY(0); })
#define NPUSH()  ({ sm->count = RX(); while(sm->count--) { *++(sm->sp) = RX(); } ACK(); })
#define NPOP()   ({ sm->count = RX(); REPLY(sm->count); while(sm->count--) { RX(*(sm->sp)--); } })
#define RX_WORD(nb_bytes) ({         \
    uint32_t w = 0;                  \
    int i;                           \
    for (i = 0; i < nb_bytes; i++) { \
        w |= (RX() << (i * 8));      \
    }                                \
    w; })
#define RX_PTR(nb_bytes) ((void*)RX_WORD(nb_bytes))
#define INTR() ({sm->ireg = RX_PTR(2); ACK(); })
#define JSR()  ({INTR(); ACLK(); })
#define LDA()  ({sm->areg = RX_PTR(2); ACK(); })
#define LDF()  ({sm->freg = RX_PTR(3); ACK(); })
#define NAFETCH() ({ sm->count = RX(); REPLY(sm->count); while (sm->count--) TX(*(sm->areg)++); })
#define NASTORE() ({ sm->count = RX(); while (sm->count--) *areg++ = rx(); ACK(); })
#define NFFETCH() ({ sm->count = rx(); reply(sm->count); while (sm->count--) TX(*(sm->freg)++); })
#define NFSTORE() ({ sm->count = rx(); while (sm->count--) *freg++ = rx(); return ack(); })

#define INTERPRET_PACKET()  ({                                          \
    /*int size =*/ RX(); /* ignore.  protocol is self-terminating. */   \
                                                                        \
    switch(RX() & 0x0F) {                                               \
    default:                                                            \
    case 0: ACK();      break;                                          \
    case 1: NPUSH();    break;                                          \
    case 2: NPOP();     break;                                          \
    case 3: JSR();      break;                                          \
    case 4: LDA();      break;                                          \
    case 5: LDF();      break;                                          \
    case 7: INTR();     break;                                          \
    case 8: NAFETCH();  break;                                          \
    case 9: NFFETCH() ; break;                                          \
    case 10: NASTORE(); break;                                          \
    case 11: NFSTORE(); break;                                          \
    }                                                                   \
    infof("\n");                                                        \
})

#define INTERPRETER() ({                        \
    while(1) {                                  \
        if (RX()) { /* ADDR */                  \
            /* not for us, just drop it. */     \
            sm->count = RX();                   \
            while (sm->count--) RX();           \
        }                                       \
        else {                                  \
            INTERPRET_PACKET();                 \
        }                                       \
    }                                           \
})

uint32_t sm_vm_tick(struct sm_vm *sm) {
    SM_RESUME(sm);
    INTERPRETER();
    SM_HALT(sm);
}


#include <stdint.h>

// Bootstrap interpreter for Staapl on ARM.
// see:
// http://balau82.wordpress.com/2010/11/30/emulating-arm-pl011-serial-ports/
// 


#define CT_ASSERT_SIZE(type, size) \
  typedef char ct_assert_size_##type[(sizeof(type)==size)?1:-1]

#define TXFF (1 << 5)
#define RXFE (1 << 4)

typedef volatile struct {
    /* 00 */ uint32_t DR;
    /* 04 */ uint32_t RSR_ECR;
    /* 08 */ uint8_t reserved1[0x10];
    /* 18 */ const uint32_t FR;
    /* 1C */ uint8_t reserved2[0x4];
    /* 20 */ uint32_t LPR;
    /* 24 */ uint32_t IBRD;
    /* 28 */ uint32_t FBRD;
    /* 2C */ uint32_t LCR_H;
    /* 30 */ uint32_t CR;
    /* 34 */ uint32_t IFLS;
    /* 38 */ uint32_t IMSC;
    /* 3c */ const uint32_t RIS;
    /* 40 */ const uint32_t MIS;
    /* 44 */ uint32_t ICR;
    /* 48 */ uint32_t DMACR;
} pl011_t;

CT_ASSERT_SIZE(pl011_t, 0x4C);

pl011_t* const uart0 = (pl011_t *)0x101F1000;
pl011_t* const uart1 = (pl011_t *)0x101F2000;
pl011_t* const uart2 = (pl011_t *)0x101F3000;

#define ME "staapl/arm/qemu: "

void uart_putchar(pl011_t *u, char c) {
    while(u->FR & TXFF);
    u->DR = c;
}
void uart_puts(pl011_t *u, const char *c) {
    while(*c) {uart_putchar(u, *c++);}
    uart_putchar(u, '\n');
    uart_putchar(u, '\r');
}
int uart_getchar(pl011_t *u) {
    while ((u->FR & RXFE) != 0);
    return u->DR;
}
void uart_echo(pl011_t *u) {
    while(1) {
        char c = uart_getchar(u);
        if (c == '\r')
            uart_putchar(u, '\n');
        // if (c == 4) return;
        uart_putchar(u, c);
    }
}
void uart_ack(pl011_t *u) {
    uart_putchar(u, 0); // zero size reply
}
void uart_interpret_packet(pl011_t *u, int size) {
    // Ignore packet size.  Protocol content is self-delimiting.
    int target = uart_getchar(u);
    if (target != 0) {
        // drop packet
        size --;
        while (size--) uart_getchar(u);
    }
    int command = uart_getchar(u);
    switch(command & 0x0F) {
    default:
    case 0: uart_ack(u); break;
#if 0
    case 1: uart_npush(u); break;
    case 2: uart_npop(u); break;
    case 3: uart_jsr(u); break;
    case 4: uart_lda(u); break;
    case 5: uart_ldf(u); break;
    case 7: uart_intr(u); break;
    case 8: uart_nafetch(u); break;
    case 9: uart_nffetch(u); break;
    case 10: uart_nastore(u); break;
    case 11: uart_nfstore(u); break;
#endif
    }

}

void uart_interpreter(pl011_t *u) {
    while(1) {
        int size = uart_getchar(u);
        if (size) uart_interpret_packet(u, size);
    }
}

void reset(void) {
    uart_puts(uart0, ME "reset()");
    uart_echo(uart0);
}

__attribute__ ((section(".vectors")))
uint32_t vectors [] = {
    0x10000,  // 64k
    (uint32_t)(&reset)
};

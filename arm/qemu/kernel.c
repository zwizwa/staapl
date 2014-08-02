#include <stdint.h>

// Bootstrap interpreter for Staapl on ARM.
// see:
// http://balau82.wordpress.com/2010/11/30/emulating-arm-pl011-serial-ports/
// 


uint8_t stack[1024];
uint8_t *sp = &stack[-1];
uint8_t *areg = 0;
uint8_t *freg = 0;
uint8_t *ireg = 0;

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

void u_tx(pl011_t *u, char c) {
    while(u->FR & TXFF);
    u->DR = c;
}
int u_rx(pl011_t *u) {
    while ((u->FR & RXFE) != 0);
    return u->DR & 0xFF;
}


// Log console
// ...
const uint8_t hexdigit[] = "0123456789ABCDEF";
void tx1(char c) { u_tx(uart1, c); }
void px(uint8_t x) {
    tx1(hexdigit[(x>>4)&0xF]);
    tx1(hexdigit[x&0xF]);
    tx1(' ');
}  
void cr(void) {
    tx1('\n');
    tx1('\r');
}


// Command console
void tx(char c) {
    tx1('T');
    tx1(':');
    px(c);
    u_tx(uart0, c);
}
uint8_t rx(void) {
    uint8_t c = u_rx(uart0);
    tx1('R');
    tx1(':');
    px(c);
    return c;
}

void reply(uint8_t n) {
    tx(0xFF); // return address
    tx(n);    // size reply
}
void ack(void) {
    reply(0);
}
void npush(void) {
    int count = rx();
    while(count--) { *++sp = rx(); }
    return ack();
}
void npop(void) {
    int count = rx();
    reply(count);
    while(count--) { tx(*sp--); }
}
uint32_t rx_word(int nb_bytes) {
    uint32_t w = 0;
    int i;
    for (i = 0; i < nb_bytes; i++) {
        w |= (rx() << (i * 8));
    }
    return w;
}
void * rx_ptr(int nb_bytes) {
    return (void*)rx_word(nb_bytes);
}
void intr(void) {
    ireg = rx_ptr(2);
    return ack();
}
void jsr(void) {
    intr();
    return ack();
}
void lda(void) {
    areg = rx_ptr(2);
    return ack();
}
void ldf(void) {
    freg = rx_ptr(3);
    return ack();
}
void nafetch(void) {
    int count = rx();
    reply(count);
    while (count--) tx(*areg++);
}
void nastore(void) {
    int count = rx();
    while (count--) *areg++ = rx();
    return ack();
}
void nffetch(void) {
    int count = rx();
    reply(count);
    while (count--) tx(*freg++);
}
void nfstore(void) {
    int count = rx();
    while (count--) *freg++ = rx();
    return ack();
}
void interpret_packet(void) {
    int size = rx(); // ignore.  protocol is self-terminating.

    int command = rx();
    switch(command & 0x0F) {
    default:
    case 0: ack(); break;
    case 1: npush(); break;
    case 2: npop(); break;
    case 3: jsr(); break;
    case 4: lda(); break;
    case 5: ldf(); break;
    case 7: intr(); break;
    case 8: nafetch(); break;
    case 9: nffetch(); break;
    case 10: nastore(); break;
    case 11: nfstore(); break;
    }
    cr();
}

void interpreter(void) {
    while(1) {
        int addr = rx();
        if (addr) {
            // not for us, just drop it.
            int count = rx();
            while (count--) rx();
        }
        else {
            interpret_packet();
        }
    }
}

void reset(void) {
    interpreter();
}

__attribute__ ((section(".vectors")))
uint32_t vectors [] = {
    0x10000,  // 64k
    (uint32_t)(&reset)
};

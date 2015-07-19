#include <stdint.h>

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

#define uart0 ((pl011_t *)0x101F1000)
#define uart1 ((pl011_t *)0x101F2000)
#define uart2 ((pl011_t *)0x101F3000)

void u_tx(pl011_t *u, char c) {
    while(u->FR & TXFF);
    u->DR = c;
}
int u_rx(pl011_t *u) {
    while ((u->FR & RXFE) != 0);
    return u->DR & 0xFF;
}

int info_putchar(int c) {
    u_tx(uart1, c);
    return 0;
}

// Command console
void comm_tx(char c) {
    u_tx(uart0, c);
}
uint8_t comm_rx(void) {
    return u_rx(uart0);
}

void reset(void) {
    /* FIXME: bss is not cleared? but qemu starts with all-zero image.*/
    /* FIXME: re-ordering variables / calls to infof() changed behavior: bug? */
    //infof(ME"%x %x\n",tx_nb,rx_nb);
    interpreter();
}

__attribute__ ((section(".vectors")))
uint32_t vectors [] = {
    0x10000,  // 64k
    (uint32_t)(&reset)
};

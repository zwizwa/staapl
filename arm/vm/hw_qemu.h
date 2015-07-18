#ifndef HW_QEMU_H
#define HW_QEMU_H

#ifndef CT_ASSERT_SIZE
#define CT_ASSERT_SIZE(type, size) \
  typedef char ct_assert_size_##type[(sizeof(type)==size)?1:-1]
#endif

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

#define uart0 ((volatile pl011_t *)0x101F1000)
#define uart1 ((volatile pl011_t *)0x101F2000)
#define uart2 ((volatile pl011_t *)0x101F3000)


static inline void u_tx(pl011_t *u, char c) {
    while(u->FR & TXFF);
    u->DR = c;
}
static inline int u_rx(pl011_t *u) {
    while ((u->FR & RXFE) != 0);
    return u->DR & 0xFF;
}

static inline int info_putchar(char c) { u_tx(uart1, c); }

void reset(void);
__attribute__ ((section(".vectors")))
uint32_t vectors [] = {
    0x10000,  // 64k
    (uint32_t)(&reset)
};


#endif // HW_QEMU_H

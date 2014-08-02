#include <stdint.h>

// Bootstrap interpreter for Staapl on ARM.
// from http://balau82.wordpress.com/2010/11/30/emulating-arm-pl011-serial-ports/

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

// #define ME "armdev/qemu-m3: "
#define ME

void uart_putchar(pl011_t *uart, char c) {
    while(uart->FR & TXFF);
    uart->DR = c;
}
void uart_puts(pl011_t *uart, const char *c) {
    while(*c) {uart_putchar(uart, *c++);}
    uart_putchar(uart, '\n');
    uart_putchar(uart, '\r');
}
int uart_getchar(pl011_t *uart) {
    while ((uart->FR & RXFE) != 0);
    return uart->DR;
}
void uart_echo(pl011_t *uart) {
    while(1) {
        char c = uart_getchar(uart);
        if (c == '\r')
            uart_putchar(uart, '\n');
        if (c == 4) return;
        uart_putchar(uart, c);
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

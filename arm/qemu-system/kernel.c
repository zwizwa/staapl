#include <stdint.h>

// Bootstrap interpreter for Staapl on ARM.
// see:
// http://balau82.wordpress.com/2010/11/30/emulating-arm-pl011-serial-ports/
//

#include "infof.h"

uint8_t stack[1024];
uint8_t *sp = &stack[-1];
uint8_t *areg = 0;
uint8_t *freg = 0;
uint8_t *ireg = 0;


void comm_tx(char c);
uint8_t comm_rx(void);

#define ME __FILE__ ": "


// Command console
uint32_t tx_nb, rx_nb;
char last_mode[] = "?";
void info_mode(char mode) {
    if (last_mode[0] == mode) return;
    last_mode[0] = mode;
    infof(" %s:",last_mode);
}
void tx(char c) {
    info_mode('T');
    infof(" %02x(%d)", c, tx_nb++);
    comm_tx(c);
}
uint8_t rx(void) {
    uint8_t c = comm_rx();
    info_mode('R');
    infof(" %02x(%d)", c, rx_nb++);
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
    (void)size;

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
    infof("\n");
}

void interpreter(void) {
    infof(ME "interpreter()\n");
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


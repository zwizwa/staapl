#include <stdint.h>
#include <unistd.h>
#include <stdlib.h>

int info_putchar(int c) {
    write(2,&c,1);
    return 0;
}

// Command console
void comm_tx(char c) {
    write(1,&c,1);
}
uint8_t comm_rx(void) {
    uint8_t c;
    if (read(0,&c,1) > 0) return c;
    exit(1);
}

void interpreter(void);
int main(void) {
    interpreter();
    return 0;
}

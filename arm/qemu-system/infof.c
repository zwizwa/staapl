#include "infof.h"
#include <stdarg.h>

/* LICENSE:

   This code (info.h and info.c) is placed in the public domain by its
   author, Tom Schouten.

   The code is based on original research on embedded software
   patterns in C and other languages, performed throughout the period
   2002 - 2015 */



void info_decimal(int d) {
    if (d < 0) { info_putchar('-'); d = -d; }
    char stack[12]; // enough for max 2^31
    char *s = stack;
    while(d) {*s++ = '0' + d % 10; d /= 10;}
    if (s == stack) info_putchar('0');
    else while(s > stack) info_putchar(*--s);
}
void info_hex(unsigned int d, int digits) {
    while(digits > 0) {
        info_putchar("0123456789abcdef"[(d >> (4*(--digits))) & 0xF]);
    }
}
void info_str(const char *c) {
    while(*c) info_putchar(*c++);
}
static inline int is_digit(int d) {
    return d >= '0' && d <= '9';
}
int infof(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    while (*fmt) {
        switch(*fmt) {
        case '%': {
            fmt++;
            /* Ignore '0'; always print leading zeros for %x, by default: %08x.
               For %d all numeric arguments are ignored. */
            int nb_digits = 8;
            while (is_digit(*fmt)) {nb_digits = *fmt-'0'; fmt++;}
            switch(*fmt) {
            case 0: break;
            case 'X':
            case 'x': fmt++; info_hex(va_arg(ap,int),nb_digits); break;
            case 'd': fmt++; info_decimal(va_arg(ap, int));      break;
            case 's': fmt++; info_str(va_arg(ap,const char*));   break;
            default:
                info_putchar('%');
                info_putchar(*fmt);
                fmt++;
                break;
            }
        }
        default:
            info_putchar(*fmt++);
            break;
        }
    }
    va_end(ap);
    return 0;  // whatever..
}

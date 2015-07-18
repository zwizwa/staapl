#ifndef INFOF_H
#define INFOF_H

/* LICENSE:

   This code (info.h and info.c) is placed in the public domain by its
   author, Tom Schouten.

   The code is based on original research on embedded software
   patterns in C and other languages, performed throughout the period
   2002 - 2015 */

/* infof() is a simplistic version of printf, only depends on
   info_putchar().  The itch this scratches is to enable printf-style
   debugging on an embedded target that does not want a malloc()
   dependency due to printf's dependency on buffered i/o. */

/* Externally defined. */
int info_putchar(int c);

/* Entry points */
int infof(const char *fmt, ...);
void info_decimal(int d);
void info_hex(unsigned int d, int digits);
void info_str(const char *c);

#endif



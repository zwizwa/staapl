#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

/* Minimalistic console to interface textual commands to Staapl monitor. */

#define BUF_SIZE 1024
char buf[BUF_SIZE];

struct word {
    const char *name;
    unsigned int addr;
};

struct word word[] = {
    {"relay!",1234},
    {}
};

int main(int argc, const char **argv) {
    const char *console = "/dev/ttyACM0";
    if (argc >= 2) {
        console = argv[1];
    }
    FILE *f = fopen(console, "w+");
    if (!f) return -1;

    unsigned int size = 0;
    unsigned int nondigits = 0;
    for(;;) {
        int c = getchar();
        if (EOF == c) return 0;
        if (isspace(c)) {
            buf[size] = 0;
            if (size > 0) {
                if (nondigits) {
                    unsigned int addr = 0;
                    for(int i=0; word[i].name; i++) {
                        if (!strcmp(word[i].name, buf)) {
                            addr = word[i].addr;
                        }
                    }
                    printf("word: %s %d\n", buf, addr);
                }
                else {
                    printf("number: %d\n", atoi(buf));
                }
            }
            size = 0;
            nondigits = 0;
        }
        else {
            if(size < BUF_SIZE) {
                buf[size++] = c;
                nondigits += !isdigit(c);
            }
        }
    }
}

#include <stdlib.h>

int main(int argc, char **argv) {
    if (argc != 3) {
        fprintf(stderr, "usage: %s <inpipe> <outpipe>\n", argv[0]);
        exit(1);
    }

    for (;;) {
        char buf[0x110];
        FILE *in  = fopen(argv[1], "r");
        FILE *out = fopen(argv[2], "w+");
    }
}

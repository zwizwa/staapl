/* This little driver is to be used in conjuncion with socat's EXEC
   pipe.  It takes a file, dumps it on stdout, reads back a reply of
   the same size and saves it as a gdb command file.

   (c) Tom Schouten 2012
   Licenced under WTFPL.
   http://en.wikipedia.org/wiki/WTFPL

 */
#include <stdlib.h>
#include <stdio.h>

int main(int argc, char **argv) {
    if (argc != 3) {
        fprintf(stderr, "usage: %s <in.bin> <out.gdb>\n", argv[0]);
        exit(1);
    }
    FILE *in  = fopen(argv[1], "r");
    FILE *out = fopen(argv[2], "w+");
    if (!in) {
        fprintf(stderr, "error opening input file %s\n", argv[1]);
        exit(2);
    }
    if (!out) {
        fprintf(stderr, "error opening output file %s\n", argv[2]);
        exit(3);
    }
    fseek(in, 0L, SEEK_END);
    long sz = ftell(in);
    fseek(in, 0L, SEEK_SET);
    if (!sz) {
        fprintf(stderr, "empty input file\n");
        exit(4);
    }
    else {
        unsigned char buf[sz];
        fread (buf, sz, 1, in);
        fwrite(buf, sz, 1, stdout);
        fflush(stdout);
        fread (buf, sz, 1, stdin);
        int i;
        fprintf(out, "set gdb_rpc={%d", buf[0]);
        for (i=2;i<sz;i++) fprintf (out, ",%d", buf[i]);
        fprintf(out, "}\n");
        fclose(out);
        fclose(in);
        exit(0);
    }
}

#include <stdio.h>
#include <stdlib.h>
int main(int argc, char **argv) {
  FILE *f = fopen(argv[1],"w");
  if (!f) {
    perror(argv[1]);
    exit(1);
  }
  unsigned char bytes[] = {0x90, 0x40, 0x7F};
  fwrite(bytes, sizeof(bytes), 1, f);
  fclose(f);
  return 0;
}

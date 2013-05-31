#include <stdio.h>
#include <unistd.h>

unsigned char gdb_request[] = {1,2,3,4};
unsigned char gdb_reply[]   = {0,0,0,0};

#define ARRAY_SIZE(a) (sizeof(a)/sizeof(a[0]))
#define FOR_ARRAY(p,a) for(p=&a[0];p<&a[ARRAY_SIZE(a)];p++)

void gdb_call() {
    /* When service is not attached, this is a nop. */
}

int main(void) {
    for(;;) {
        unsigned char *e;

        printf("request:");
        FOR_ARRAY(e, gdb_reply) {
            printf(" %d", *e);

        }
        printf("\n");

        gdb_call();

        printf("reply:  ");
        FOR_ARRAY(e, gdb_reply) {
            printf(" %d", *e);
        }
        printf("\n");

        sleep(1);
    }
    return 0;
}

#include <usb.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>

#include "usb_rpc.h"

/* CLIENT */
#define VID 0x04d8
#define PID 0x0033
#define ENDPOINT_IN  0x81
#define ENDPOINT_OUT 0x01
#define TIMEOUT 5000
#define BUFSIZE 64

static void rpc_dispatch(struct usb_rpc *rpc) {
    write(1, rpc, sizeof(*rpc));
    read(1, rpc, sizeof(*rpc));
}

int pk2_cmd(struct usb_rpc *rpc,
            u8 *request, int request_size,
            u8 *reply,   int reply_size) {

    rpc->token = USB_RPC_OUT;
    rpc->arg.io.size = 64;
    rpc->arg.io.timeout = TIMEOUT;
    rpc->arg.io.endpoint = ENDPOINT_OUT;

    memset(&(rpc->buf), 0xad, rpc->arg.io.size);  // END_OF_BUFFER
    memcpy(&(rpc->buf), request, request_size);

    rpc_dispatch(rpc);
    if (USB_RPC_REPLY != rpc->token) return 1;
    if (reply_size) {
        rpc->token = USB_RPC_IN;
        rpc->arg.io.size = 64;
        rpc->arg.io.timeout = TIMEOUT;
        rpc->arg.io.endpoint = ENDPOINT_IN;
        bzero(rpc->buf, rpc->arg.io.size);
        rpc_dispatch(rpc);
        if (USB_RPC_REPLY != rpc->token) return 2;
        memcpy(reply, &rpc->buf, reply_size);
    }
    return 0;
}


#define CMD(rpc, reply_buf, cmd_init...) {                              \
        u8 _cmd[] = cmd_init;                                           \
        pk2_cmd(rpc, _cmd, sizeof(_cmd), reply_buf, sizeof(reply_buf)); }


#define CMD_DBG(rpc, size, cmd_init...) {                   \
        u8 _reply[size]; CMD(rpc, _reply, cmd_init);        \
        hexdump(_reply, sizeof(_reply), sizeof(_reply)); }

#define FIRMWARE_VERSION 0x76

int check_dev(struct usb_rpc *rpc) {
    CMD_DBG(rpc, 3, {FIRMWARE_VERSION});
    return 0;
}

int main(int argc, char **argv){
    struct usb_rpc rpc = {};
    rpc.token = USB_RPC_OPEN_VID_PID;
    rpc.arg.vid_pid.vid = VID;
    rpc.arg.vid_pid.pid = PID;
    rpc_dispatch(&rpc);
    int rv = check_dev(&rpc);

    rpc.token = USB_RPC_CLOSE;
    rpc_dispatch(&rpc);
    return rv;
}

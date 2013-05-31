#include <usb.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>

/* Generic adapter for PK2 */

/* stdio adapter for PK2.

   PK2 uses a simple vendor-specific configuration (2).  There is also
   a HID config (1).

   The vendor-specific config has a packet-oriented RPC protocol on
   EP1.  Each transaction sends 64 bytes to the device EP1:OUT.  If
   the command returns data, PK2 will return 64 bytes on EP1:IN.

   For a summary of commands see PICkit2SourceGuidePCv2-52FWv2-32.pdf

   md5:  10625f66f0977f6a265e0341b8984d16
   sha1: 1edf7a076cec65d69821b3d53317c16fec8823fb

   This 64-byte transaction is what is exposed on stdio.  The
   difficulty is in knowing whether there is a reply or not.  This can
   either be done by this program (interpreting the data) or by the
   user.

   To make things general, this application uses an extra layer,
   exposing the calls to libusb.

*/
#include "usb_rpc.h"

/* STATE */
static struct usb_dev_handle *dev = NULL;
static bool verbose = true;


static void rpc_clear(struct usb_rpc *rpc) {
    bzero(rpc, sizeof(*rpc));
    rpc->version = USB_RPC_VERSION;
}
static void rpc_error(struct usb_rpc *rpc, s32 e) {
    rpc_clear(rpc);
    rpc->token = USB_RPC_ERROR;
    rpc->arg.s32 = e;
}
static struct usb_dev_handle *dev_open_dev(struct usb_device *usb_dev) {
    struct usb_dev_handle *dev = usb_open(usb_dev);
    if (dev) {
        /* Config 2 is vendor-specific, 1 is HID. */
        int conf = 2;
        int err = usb_set_configuration(dev, conf);
        if (err) {
            fprintf(stderr, "Can't set configuration %d\n", conf);
            usb_close(dev);
        }
    }
    return dev;
}

static void init_libusb(void) {
    static bool initialized = false;
    if (!initialized) {
        usb_init();
        usb_find_busses();
        usb_find_devices();
        initialized = true;
    }
}

static struct usb_dev_handle *dev_open(u16 VID, u16 PID) {
    init_libusb();

    struct usb_bus *busses = usb_get_busses();
    struct usb_bus *bus;

    /* Find DEV by VID:PID */
    for (bus = busses; bus; bus = bus->next) {
        struct usb_device *dev;
        for (dev = bus->devices; dev; dev = dev->next) {
            if ((VID == dev->descriptor.idVendor) &&
                (PID == dev->descriptor.idProduct)) {
                fprintf(stderr,
                        "Found %04x:%04x at /dev/bus/usb/%s/%s\n",
                        VID, PID,
                        bus->dirname,
                        dev->filename);
                return dev_open_dev(dev);
            }
        }
    }
    if (verbose) {
        fprintf(stderr,
                "Cand find %04x:%04x\n",
                VID, PID);
    }
    return NULL;
}
static void dev_close(struct usb_rpc *rpc) {
    if (dev) usb_close(dev);
    dev = NULL;
    rpc_clear(rpc);
}

static void dev_open_vid_pid(struct usb_rpc *rpc) {
    if (dev) usb_close(dev);
    dev = dev_open(rpc->arg.vid_pid.vid,
                   rpc->arg.vid_pid.pid);
    if (!dev) {
        if (verbose) {
            fprintf(stderr, "Can't open %04x:%04x\n",
                    rpc->arg.vid_pid.vid,
                    rpc->arg.vid_pid.pid);
        }
        rpc_error(rpc, USB_RPC_ERROR_OPEN);
    }
    else {
        rpc_clear(rpc);
    }
}
static void no_device(struct usb_rpc *rpc) {
    if (verbose) {
        fprintf(stderr, "No device open\n");
    }
    rpc_error(rpc, USB_RPC_ERROR_NODEV);
}
static void dev_out(struct usb_rpc *rpc) {
    if (!dev) { no_device(rpc); return; }
    if (verbose) {
        fprintf(stderr, "-> dev\n");
        hexdump(rpc->buf, rpc->arg.io.size, 8);
    }
    int rv = usb_interrupt_write(dev,
                                 rpc->arg.io.endpoint,
                                 (char*)rpc->buf,
                                 rpc->arg.io.size,
                                 rpc->arg.io.timeout);
    if (rv != rpc->arg.io.size) {
        if (verbose) {
            fprintf(stderr, "usb_interrupt_write: %d\n", rv);
        }
        rpc_error(rpc, rv);
        return;
    }
    rpc_clear(rpc);
}

static void dev_in(struct usb_rpc *rpc) {
    if (!dev) { no_device(rpc); return; }
    int rv = usb_interrupt_read(dev,
                                rpc->arg.io.endpoint,
                                (char*)rpc->buf,
                                rpc->arg.io.size,
                                rpc->arg.io.timeout);
    if (rv != rpc->arg.io.size) {
        if (verbose) {
            fprintf(stderr, "usb_interrupt_read: %d\n", rv);
        }
        rpc_error(rpc, rv);
        return;
    }
    rpc->token = USB_RPC_REPLY;
    if (verbose) {
        fprintf(stderr, "<- dev\n");
        hexdump(rpc->buf, rpc->arg.io.size, 8);
    }
}

static void rpc_dispatch(struct usb_rpc *rpc) {
    if (rpc->version != USB_RPC_VERSION) {
        fprintf(stderr, "USB_RPC: version mismatch\n");
        rpc_error(rpc, USB_RPC_ERROR_VERSION);
        return;
    }
    enum usb_rpc_token t = rpc->token;
    switch(t) {
    case USB_RPC_CLOSE:        dev_close(rpc);        break;
    case USB_RPC_OPEN_VID_PID: dev_open_vid_pid(rpc); break;
    case USB_RPC_OUT:          dev_out(rpc);          break;
    case USB_RPC_IN:           dev_in(rpc);           break;

    case USB_RPC_REPLY:
    default:
        if (verbose) {
            fprintf(stderr, "USB_RPC: Can't handle token %04x\n", t);
        }
        rpc_error(rpc, USB_RPC_ERROR_TOKEN);
        break;
    }
}
// socat EXEC:./pk2 EXEC:./usb_rpc
int main(int argc, char **argv){
    u8 buf[sizeof(struct usb_rpc)];
    struct usb_rpc *rpc = (void*)&buf;
    for(;;) {
        int rv;
        rv = read(0, rpc, sizeof(*rpc));
        if (rv != sizeof (*rpc)) {
            if (verbose) {
                if (rv) {
                    fprintf(stderr, "USB_RPC: read() -> %d\n", rv);
                }
                else {
                    fprintf(stderr, "USB_RPC: exit\n");
                }
                if (dev) usb_close(dev);
                exit(rv);
            }
        }
        if (verbose) {
            fprintf(stderr, "---------------------------\n");
            fprintf(stderr, "USB_RPC: command token %04x\n", rpc->token);
        }
        rpc_dispatch(rpc);
        if (verbose) {
            fprintf(stderr, "USB_RPC: command reply %04x\n", rpc->token);
        }
        write(1, rpc, sizeof(*rpc));
    }
}

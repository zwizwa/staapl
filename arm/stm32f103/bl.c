#include <stdlib.h>
#include <string.h>
#include <libopencm3/stm32/rcc.h>
#include <libopencm3/stm32/gpio.h>
#include <libopencm3/usb/usbd.h>
#include <libopencm3/usb/cdc.h>

// generic code in cdcacm_desc.c
void cdcacm_set_config_with_callbacks(usbd_device *, void*, void*);
usbd_device *cdcacm_init(void *set_config, const char * const *usb_strings);




/* CDCACM */
usbd_device *usb_serial;
static const char * usb_strings[] = {
    "Zwizwa","Staapl-stm32f103","v1.0"
};

/* I/O functions are stored in RAM to be replaced by application later. */
typedef uint32_t (*serial_fn_read)(void);
typedef void (*serial_fn_write)(uint32_t n);
struct serial_io {
    serial_fn_read read;
    serial_fn_write write;
};
/* These operate on the buffers below. */
uint8_t rx_buf[64];
uint8_t tx_buf[64];
uint32_t tx_buf_sema;



/* Simple echo */
static void echo_write(uint32_t n) {
    rx_buf[n] = 0;
}
static uint32_t echo_read(void)  {
    uint32_t n = 0;
    while(rx_buf[n]) { tx_buf[n] = rx_buf[n]; n++; }
    rx_buf[0] = 0;
    return n;
}
const struct serial_io echo_io = { .read  = echo_read, .write = echo_write };


/* Staapl interface. */
// this should use packet buffering in case host doesn't write() everything at once
uint32_t comm_tx_n;
uint32_t comm_rx_n;
void interpret_route(void);
void comm_tx(char c) {
    tx_buf[comm_tx_n++] = c;
}
uint8_t comm_rx(void) {
    return rx_buf[comm_rx_n++];
}
static void comm_write(uint32_t n) {
    //if (n && rx_buf[0] > 0) {
    //    for(char *c = "Staapl binary messages only.\r\n"; *c; c++) comm_tx(*c);
    //    return;
    //}
    comm_rx_n = n;
    interpret_route();
}
static uint32_t comm_read(void)  {
    uint32_t n = comm_tx_n;
    comm_tx_n = 0;
    return n;
}
const struct serial_io comm_io = { .read  = comm_read, .write = comm_write };


//const struct serial_io *io = &echo_io;
const struct serial_io *io = &comm_io;



/* Send len bytes from tx_buf */
static void data_tx(uint32_t len) {
    tx_buf_sema++;
    usbd_ep_write_packet(usb_serial, 0x82, tx_buf, len);
}
/* Poll output state machine. */
static void poll_data_tx(void) {
    if (tx_buf_sema > 0) return; // busy
    uint32_t len = io->read();
    if (len) data_tx(len);
}
/* USB transmission complete. */
static void data_tx_cb(usbd_device *usbd_dev, uint8_t ep) {
    if (usbd_dev != usb_serial) return;
    tx_buf_sema--;
    /* Immediately check if there's more. */
    poll_data_tx();
}
/* Receive bytes into rx_buf, return number. */
static uint32_t data_rx(void) {
    return usbd_ep_read_packet(usb_serial, 0x01, rx_buf, sizeof(rx_buf));
}
/* USB has data available.  Push it to state machine + poll TX. */
static void data_rx_cb(usbd_device *usbd_dev, uint8_t ep) {
    if (usbd_dev != usb_serial) return;
    io->write(data_rx());
    /* Immediately check if there's anything available. */
    poll_data_tx();
}


/* Callbacks & init */
static void usb_reset(void) {
    //io = &comm_io;
}
static void cdcacm_set_config(usbd_device *usbd_dev, uint16_t wValue) {
    (void)wValue;
    cdcacm_set_config_with_callbacks(usbd_dev, data_rx_cb, data_tx_cb);
    usbd_register_reset_callback(usbd_dev, usb_reset);
}
int main(int argc, char **argv) {
    rcc_clock_setup_in_hse_8mhz_out_72mhz();
    rcc_periph_clock_enable(RCC_GPIOA);
    rcc_periph_clock_enable(RCC_GPIOB);
    //rcc_periph_clock_enable(RCC_GPIOC);
    rcc_periph_clock_enable(RCC_AFIO);
    usb_serial = cdcacm_init(cdcacm_set_config, usb_strings);
    for(;;) {
        usbd_poll(usb_serial);
        poll_data_tx(); // for async data
    }
}

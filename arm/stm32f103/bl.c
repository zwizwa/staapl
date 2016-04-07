#include <stdlib.h>
#include <string.h>
#include <libopencm3/stm32/rcc.h>
#include <libopencm3/stm32/gpio.h>
#include <libopencm3/usb/usbd.h>
#include <libopencm3/usb/cdc.h>


/* CDCACM */
void cdcacm_set_config_with_callbacks(usbd_device *, void*, void*);
usbd_device *cdcacm_init(void *set_config, const char * const *usb_strings);
usbd_device *usb_serial;
static const char * usb_strings[] = {
    "Zwizwa","Staapl-stm32f103","v1.0"
};

/* USB endpoint buffers. */
static uint8_t  rx_buf[64];
static uint32_t rx_r = 0;
static uint32_t rx_n = 0;
static uint8_t  tx_buf[64];
static uint32_t tx_w = 0;
static uint32_t tx_sema;

/* Block read/write */
static void poll_tx(void) {
    if (tx_sema > 0) return; // USB busy
    if (tx_w) {
        tx_sema++;
        usbd_ep_write_packet(usb_serial, 0x82, tx_buf, tx_w);
        tx_w = 0;
    }
}
static void data_tx_cb(usbd_device *usbd_dev, uint8_t ep) {
    if (usbd_dev != usb_serial) return;
    tx_sema--;
    poll_tx();
}
static void data_rx_cb(usbd_device *usbd_dev, uint8_t ep) {
    if (usbd_dev != usb_serial) return;
    rx_n = usbd_ep_read_packet(usb_serial, 0x01, rx_buf, sizeof(rx_buf));
    rx_r = 0;
    poll_tx();
}

/* Character read/write */
void poll(void) {
    usbd_poll(usb_serial);
    poll_tx();
}
void comm_tx(char c) {
    tx_buf[tx_w++] = c;
    poll_tx();
}
uint8_t comm_rx(void) {
    for(;;) {
        if (rx_r < rx_n) { return rx_buf[rx_r++]; }
        poll();
    }
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
void interpreter(void);
int main(int argc, char **argv) {
    rcc_clock_setup_in_hse_8mhz_out_72mhz();
    rcc_periph_clock_enable(RCC_GPIOA);
    rcc_periph_clock_enable(RCC_GPIOB);
    //rcc_periph_clock_enable(RCC_GPIOC);
    rcc_periph_clock_enable(RCC_AFIO);
    usb_serial = cdcacm_init(cdcacm_set_config, usb_strings);
    interpreter();
}

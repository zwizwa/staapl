#include <usb.h>
#include <stdio.h>
int main(int argc, int **argv){
    struct usb_bus *busses;

    
    usb_init();
    usb_find_busses();
    usb_find_devices();
    
    busses = usb_get_busses();
        


    struct usb_bus *bus;
    int c, i, a;
    
    /* ... */
    
    for (bus = busses; bus; bus = bus->next) {
        struct usb_device *dev;
        
        for (dev = bus->devices; dev; dev = dev->next) {
            /* Check if this device is a printer */
            printf("device %04x:%04x\n", 
                   dev->descriptor.idVendor,
                   dev->descriptor.idProduct);
            if (dev->descriptor.bDeviceClass == 7) {
                /* Open the device, claim the interface and do your processing */
            }
            
            /* Loop through all of the configurations */
            for (c = 0; c < dev->descriptor.bNumConfigurations; c++) {
    				/* Loop through all of the interfaces */
                for (i = 0; i < dev->config[c].bNumInterfaces; i++) {
                    /* Loop through all of the alternate settings */
                    for (a = 0; a < dev->config[c].interface[i].num_altsetting; a++) {
                        /* Check if this interface is a printer */
                        if (dev->config[c].interface[i].altsetting[a].bInterfaceClass == 7) {
                            /* Open the device, set the alternate setting, claim the interface and do your processing */
                            printf("configuration!\n");

                        }
                    }
                }
            }
        }
    }
}

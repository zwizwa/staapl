macro

\ USB descriptor field names for compilation to Flash
\ Both USB descriptors and PIC Flash are little endian.
\ Can't use ",," because words might not be word-address aligned  
  
\ ref: http://www.beyondlogic.org/usbnutshell/usb5.shtml


: w, dup >m #xFF and , m> 8 >>> , ;
  
: bLength , ;
: bDescriptorType , ;    
  
\ DEVICE descriptor fields
  
: bcdUSB w, ;
: bDeviceClass , ;
: bDeviceSubClass , ;
: bDeviceProtocol , ;
: bMaxPacketSize , ;
: idVendor w, ;
: idProduct w, ;
: bcdDevice w, ;
: iManufacturer , ;
: iProduct , ;
: iSerialNumber , ;    
: bNumConfigurations , ;

\ CONFIGURATION
    
: wTotalLength w, ;
: bNumInterfaces , ;
: bConfigurationValue , ;
: iConfiguration , ;  
: bmAttributes , ;
: bMaxPower , ;    

\ INTERFACE    
: bInterfaceNumber , ;
: bAlternateSetting , ;
: bNumEndpoints , ;
: bInterfaceClass , ;
: bInterfaceSubClass , ;
: bInterfaceProtocol , ;
: iInterface , ;    

\ ENDPOINT
: bEndpointAddress , ;
: wMaxPacketSize w, ;
: bInterval , ;    



\ Descriptors are tables prefixed with a size field.
: descriptor-size , ;


\ Compile string descriptor:  sym --
: s,
    sym>bin          dup >m
    l:length 2 * 2 + dup >m \ descriptor length
       descriptor-size      \ transport wrapper
    m> bLength
    3  bDescriptorType
    m> ' w, for-list ; \ FIXME: do proper unicode translation

\ endpoing descriptor macros
: mBulkEndpoint | addr |
    7      bLength
    5      bDescriptorType
    addr   bEndpointAddress
    #x02   bmAttributes     \ BULK
    64     wMaxPacketSize
    0      bInterval
    ;
: mInterruptEndpoint | addr |
    7      bLength
    5      bDescriptorType
    addr   bEndpointAddress
    #x03   bmAttributes     \ INTERRUPT
    64     wMaxPacketSize
    10     bInterval  \ 10 ms
    ;   
forth

  
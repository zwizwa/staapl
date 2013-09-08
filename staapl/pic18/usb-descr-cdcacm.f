staapl pic18/string
staapl pic18/route
load usb-fields.f

\ Flat USB descriptor layout.  This is a little raw, but the manual
\ effort required to fill out lengths and string indices pays back in
\ ease of debugging and specs cross-referencing.


\ http://www.beyondlogic.org/usbnutshell/usb5.shtml
\ http://www.usb.org/developers/devclass_docs/usbcdc11.pdf  
    
: device-descriptor \ - lo hi
    table->
    18 descriptor-size
    \ -----------------
    
    18     bLength
    1      bDescriptorType
    #x110  bcdUSB
    0      bDeviceClass \ Defined at interface level
    0      bDeviceSubClass
    0      bDeviceProtocol
    64     bMaxPacketSize
    #x05F9 idVendor
    #xFFFF idProduct
    0      bcdDevice
    \ Strings
    1      iManufacturer
    2      iProduct
    3      iSerialNumber
    1      bNumConfigurations

: configuration-descriptor \ n -- lo hi
    drop \ FIXME: only support one configuration

    table->
    9 9 + 7 + 7 +
    36 +
    descriptor-size
    \ -----------------
    
    \ CONFIGURATION
    9      bLength
    2      bDescriptorType

    9 9 + 7 + 7 +
    36 +
    wTotalLength
    
    5      bNumInterfaces
    1      bConfigurationValue
    0      iConfiguration
    #xA0   bmAttributes \ remote wakeup
    #x32   bMaxPower    \ 100 mA
    
    \ INTERFACE
    9      bLength
    4      bDescriptorType
    0      bInterfaceNumber
    0      bAlternateSetting
    2      bNumEndpoints
    #xFF   bInterfaceClass \ Vendor-specific
    0      bInterfaceSubClass
    0      bInterfaceProtocol
    0      iInterface

    \ ENDPOINT
    7      bLength
    5      bDescriptorType
    #x81   bEndpointAddress \ IN1
    #x02   bmAttributes     \ BULK
    64     wMaxPacketSize
    0      bInterval
    
    \ ENDPOINT
    7      bLength
    5      bDescriptorType
    #x01   bEndpointAddress \ OUT1
    #x02   bmAttributes     \ BULK
    64     wMaxPacketSize
    0      bInterval


    \ padding: dummy interfaces

    \ INTERFACE
    9      bLength
    4      bDescriptorType
    1      bInterfaceNumber
    0      bAlternateSetting
    0      bNumEndpoints
    #xFF   bInterfaceClass \ Vendor-specific
    0      bInterfaceSubClass
    0      bInterfaceProtocol
    0      iInterface
    \ INTERFACE
    9      bLength
    4      bDescriptorType
    2      bInterfaceNumber
    0      bAlternateSetting
    0      bNumEndpoints
    #xFF   bInterfaceClass \ Vendor-specific
    0      bInterfaceSubClass
    0      bInterfaceProtocol
    0      iInterface
    \ INTERFACE
    9      bLength
    4      bDescriptorType
    3      bInterfaceNumber
    0      bAlternateSetting
    0      bNumEndpoints
    #xFF   bInterfaceClass \ Vendor-specific
    0      bInterfaceSubClass
    0      bInterfaceProtocol
    0      iInterface
    \ INTERFACE
    9      bLength
    4      bDescriptorType
    4      bInterfaceNumber
    0      bAlternateSetting
    0      bNumEndpoints
    #xFF   bInterfaceClass \ Vendor-specific
    0      bInterfaceSubClass
    0      bInterfaceProtocol
    0      iInterface

    
    
\ -- lo hi
: string-languages table-> 4 , 4 , 3 , #x0409 w,  \ US English
: snull            table-> 4 , 2 , 3 , 

: string-1 table-> ` Zwizwa s, ;
: string-2 table-> ` Staapl s, ;
: string-3 table-> ` um0 s, ;  \ udev maps this to tty-um0
    
: string-descriptor \ n - lo hi
    3 and route
    string-languages .
    string-1 .
    string-2 .
    string-3 ;

    
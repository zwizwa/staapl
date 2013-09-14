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
    #xFFF0 idProduct
    0      bcdDevice
    \ Strings
    1      iManufacturer
    2      iProduct
    3      iSerialNumber
    1      bNumConfigurations

macro
: cd-size
    9 \ config
    9 + 5 + 5 + 4 + 5 + 7 + \ control interface
    9 + 7 + 7 + \ data interface
    ;
forth
    
: configuration-descriptor \ n -- lo hi
    drop \ FIXME: only support one configuration

    table-> cd-size descriptor-size
    \ -----------------
    
    \ CONFIGURATION
    9      bLength
    2      bDescriptorType

    cd-size wTotalLength
    
    2      bNumInterfaces
    1      bConfigurationValue
    0      iConfiguration
    #xA0   bmAttributes \ remote wakeup
    #x32   bMaxPower    \ 100 mA
    
    \ INTERFACE: communication
    9      bLength
    4      bDescriptorType
    0      bInterfaceNumber
    0      bAlternateSetting
    1      bNumEndpoints
    #x02   bInterfaceClass     \ CDC
    #x02   bInterfaceSubClass  \ ACM
    0      bInterfaceProtocol
    0      iInterface

    \ Class-specific header functional descriptor
    5      bLength
    #x24   bDescriptorType \ Indicates that a CDC descriptor applies to an interface
    #x00   ,               \ Header functional descriptor subtype
    #x0110 w,

    \ Class-specific call management functional descriptor
    5      bLength
    #x24   bDescriptorType \ Indicates that a CDC descriptor applies to an interface
    #x01   ,               \ Call management functional descriptor subtype
    #x01   ,               \ Device handles call management itself
    #x00   ,               \ No associated data interface

    \ Class-specific abstract control management functional descriptor
    4      bLength
    #x24   bDescriptorType \ Indicates that a CDC descriptor applies to an interface
    #x02   ,               \ Abstract control management descriptor subtype
    #x02   ,               \ Device supports the request combination of SetLineCoding, GetLineCoding and SetControlLineState.

    \ Class-specific union functional descriptor with one slave interfac
    5      bLength
    #x24   bDescriptorType \ Indicates that a CDC descriptor applies to an interface
    #x06   ,               \ Union descriptor subtype
    0      ,               \ Number of master interface is #0
    1      ,               \ First slave interface is #1

    \ Notification endpoint standard descriptor
    #x82   mInterruptEndpoint \ IN2

    \ INTERFACE: data
    9      bLength
    4      bDescriptorType
    1      bInterfaceNumber
    0      bAlternateSetting
    2      bNumEndpoints
    #x0A   bInterfaceClass     \ CDC data
    #x00   bInterfaceSubClass
    0      bInterfaceProtocol
    0      iInterface
    
    \ ENDPOINTS
    #x01   mBulkEndpoint \ OUT1
    #x81   mBulkEndpoint \ IN1

    
    
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

    
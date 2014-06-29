#lang racket/base
(require "usb-comp.rkt")
(provide
 usb-device
 DeviceDescriptor
 ConfigurationDescriptor
 (all-defined-out))

;; Standard USB spec descriptor field names.
(Fields

  (bLength             byte)
  (bDescriptorType     byte)    
  
  ;; DEVICE descriptor fields
  
  (bcdUSB              word)
  (bDeviceClass        byte)
  (bDeviceSubClass     byte)
  (bDeviceProtocol     byte)
  (bMaxPacketSize      byte)
  (idVendor            word)
  (idProduct           word)
  (bcdDevice           word)
  (iManufacturer       istring)
  (iProduct            istring)
  (iSerialNumber       istring)
  (bNumConfigurations  byte)

  ;; CONFIGURATION
    
  (wTotalLength        word)
  (bNumInterfaces      byte)
  (bConfigurationValue byte)
  (iConfiguration      byte)  
  (bmAttributes        byte)
  (bMaxPower           byte)

  ;; INTERFACE    
  (bInterfaceNumber    byte)
  (bAlternateSetting   byte)
  (bNumEndpoints       byte)
  (bInterfaceClass     byte)
  (bInterfaceSubClass  byte)
  (bInterfaceProtocol  byte)
  (iInterface          byte)

  ;; ENDPOINT
  (bEndpointAddress    byte)
  (wMaxPacketSize      word)
  (bInterval           byte))


(define (BulkEndpoint
         #:bEndpointAddress [addr #f])
  (Descriptor
   (bLength 7);; Length is verified
   (bDescriptorType 5)
   (bEndpointAddress addr)
   (bmAttributes #x02) ;; BULK
   (wMaxPacketSize 64)
   (bInterval 0)))


(define (InterruptEndpoint
         #:bEndpointAddress [addr #f])
  (Descriptor
   (bLength 7);; Length is verified
   (bDescriptorType 5)
   (bEndpointAddress addr)
   (bmAttributes #x03) ;; BULK
   (wMaxPacketSize 64)
   (bInterval 10)))
          
           
(define (DeviceDescriptor
         #:bMaxPacketSize [mps 64]
         #:idVendor       [iv #x05F9]
         #:idProduct      [ip #xFFF0]
         #:iManufacturer  [m "Zwizwa"]
         #:iProduct       [p "Staapl"]
         #:iSerialNumber  [s "um0"]
         )
  (Descriptor
   (bLength 18)
   (bDescriptorType 1)
   (bcdUSB #x110)
   (bDeviceClass 0)  ;; Defined at interface level
   (bDeviceSubClass 0)
   (bDeviceProtocol 0)
   (bMaxPacketSize mps)
   (idVendor iv)
   (idProduct ip)
   (bcdDevice 0)
   (iManufacturer m)
   (iProduct p)
   (iSerialNumber s)
   (bNumConfigurations 1)))
  
  
  
(define (ConfigurationDescriptor
         #:wTotalLength [tl -1]
         #:bNumInterfaces [ni 2]
         )
         
  (Descriptor
   (bLength 9)
   (bDescriptorType 2)
   (wTotalLength tl)
   (bNumInterfaces ni)
   (bConfigurationValue 1)
   (iConfiguration 0)   ;; Not defined
   (bmAttributes #xA0)  ;; Remote wakeup
   (bMaxPower #x32)))   ;; 100mA
   
   
(define (InterfaceDescriptor
         #:bInterfaceNumber [in 0]
         #:bNumEndpoints [ne #f]
         #:bInterfaceClass [ic #x02] ;; CDC
         #:bInterfaceSubClass [isc #x02] ;; ACM
         )
  (Descriptor
   (bLength 9)
   (bDescriptorType 4)
   (bInterfaceNumber in)
   (bAlternateSetting 0)
   (bNumEndpoints ne)
   (bInterfaceClass ic)
   (bInterfaceSubClass isc)
   (bInterfaceProtocol 0)
   (iInterface 0)
   ))
   


(define (ConfigurationDescriptorCDC)
  (TotalLength (nb_bytes)

     (ConfigurationDescriptor
      #:wTotalLength nb_bytes)

     ;; INTERFACE: communication
     (InterfaceDescriptor
      #:bInterfaceNumber   0
      #:bInterfaceClass    #x02  ;; CDC
      #:bInterfaceSubClass #x02  ;; ACM
      #:bNumEndpoints      1)

     ;; Class-specific header functional descriptor
     (Descriptor 
      (bLength 5)
      (bDescriptorType #x24) ;; Indicates that a CDC descriptor applies to an interface
      (byte #x00)            ;; Header functional descriptor subtype
      (word #x0110))

     ;; Class-specific call management functional descriptor
     (Descriptor
      (bLength 5)
      (bDescriptorType #x24) ;; Indicates that a CDC descriptor applies to an interface
      (byte #x01)            ;; Call management functional descriptor subtype
      (byte #x01)            ;; Device handles call management itself
      (byte #x00))           ;; No associated data iterface
      
     ;; Class-specific abstract control management functional descriptor
     (Descriptor
      (bLength 4)
      (bDescriptorType #x24) ;; Indicates that a CDC descriptor applies to an interface
      (byte #x02)            ;; Abstract control management descriptor subtype
      (byte #x00))           ;; Don't support any request, FIXME: still get 22,21,22 interface requests!

     ;; Class-specific union functional descriptor with one slave interfac
     (Descriptor
      (bLength 5)
      (bDescriptorType #x24) ;; Indicates that a CDC descriptor applies to an interface
      (byte #x06)            ;; Union descriptor subtype
      (byte 0)               ;; Number of master interface is #0
      (byte 1))              ;; First slave interface is #1

     (InterruptEndpoint
      #:bEndpointAddress #x82)

     ;; INTERFACE: communication
     (InterfaceDescriptor
      #:bInterfaceNumber   1
      #:bInterfaceClass    #x0A  ;; CDC data
      #:bInterfaceSubClass #x00
      #:bNumEndpoints      2)
     (BulkEndpoint #:bEndpointAddress #x01) ;; OUT1
     (BulkEndpoint #:bEndpointAddress #x81) ;; IN1
     ))
     
     

      
  

#lang racket/base

;; USB burocracy hoop-jumping

;; Some limitations:
;; - one configuration, possibly multiple interfaces

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
  (bDescriptorSubtype  byte)    
  
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
  (bInterval           byte)

  ;; MIDI
  (bcdMSC              word)
  (bJackType           byte)
  (bJackID             byte)
  (BaSourceID          byte)
  (BaSourcePin         byte)
  (iJack               byte)
  (bElementID          byte)
  (bNrInputPins        byte)
  (bNrOutputPins       byte)
  (bInTerminalLink     byte)
  (bOutTerminalLink    byte)
  (bElCapSize          byte)
  (bmElementCaps       byte)
  (iElement            byte)

  )


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
         #:bNumConfigurations [nc 1]
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
   (bNumConfigurations nc)))
  
  
  
(define (ConfigurationDescriptor
         #:wTotalLength [tl -1]
         #:bNumInterfaces [ni 2]
         #:bConfigurationValue [cv 1]
         )
         
  (Descriptor
   (bLength 9)
   (bDescriptorType 2)
   (wTotalLength tl)
   (bNumInterfaces ni)
   (bConfigurationValue cv)
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

(define (InterfaceDescriptorCDC
         #:bEndpointAddress [ea #x82]
         #:bInterfaceNumber [in 0])

  ;; INTERFACE: communication
  (InterfaceDescriptor
   #:bInterfaceNumber   in
   #:bInterfaceClass    #x02  ;; CDC
   #:bInterfaceSubClass #x02  ;; ACM
   #:bNumEndpoints      1)
  
  ;; Class-specific header functional descriptor
  (Descriptor 
   (bLength 5)
   (bDescriptorType #x24)    ;; Indicates that a CDC descriptor applies to an interface
   (bDescriptorSubtype #x00)  ;; Header functional descriptor subtype
   (word #x0110))
  
  ;; Class-specific call management functional descriptor
  (Descriptor
   (bLength 5)
   (bDescriptorType #x24)    ;; Indicates that a CDC descriptor applies to an interface
   (bDescriptorSubtype #x01) ;; Call management functional descriptor subtype
   (byte #x01)            ;; Device handles call management itself
   (byte #x00))           ;; No associated data iterface
  
  ;; Class-specific abstract control management functional descriptor
  (Descriptor
   (bLength 4)
   (bDescriptorType #x24)    ;; Indicates that a CDC descriptor applies to an interface
   (bDescriptorSubtype #x02) ;; Abstract control management descriptor subtype
   (byte #x00))           ;; Don't support any request, FIXME: still get 22,21,22 interface requests!
  
  ;; Class-specific union functional descriptor with one slave interfac
  (Descriptor
   (bLength 5)
   (bDescriptorType #x24) ;; Indicates that a CDC descriptor applies to an interface
   (bDescriptorSubtype #x06) ;; Union descriptor subtype
   (byte 0)               ;; Number of master interface is #0
   (byte 1))              ;; First slave interface is #1
  
  (InterruptEndpoint
   #:bEndpointAddress ea))

(define (InterfaceDescriptorCDCdata
         #:bEndpointAddressIN  [eaIN  #x81] ;; IN1
         #:bEndpointAddressOUT [eaOUT #x01] ;; OUT1
         #:bInterfaceNumber [in 1])

  ;; INTERFACE: communication
  (InterfaceDescriptor
   #:bInterfaceNumber   in
   #:bInterfaceClass    #x0A  ;; CDC data
   #:bInterfaceSubClass #x00
   #:bNumEndpoints      2)
  (BulkEndpoint #:bEndpointAddress eaOUT)
  (BulkEndpoint #:bEndpointAddress eaIN)
  )


(define-syntax-rule (configuration interface ...)
  (TotalLength (nb_bytes)
    (ConfigurationDescriptor
     #:wTotalLength nb_bytes
     #:bNumInterfaces (length '(interface ...)))
    interface ...))
  

     
     

;; http://www.usb.org/developers/devclass_docs/audio10.pdf
;; http://www.usb.org/developers/devclass_docs/midi10.pdf

;; Of course this is way more complicated than it needs to be.  If
;; anything, the below is optimized for single input / output - we'll
;; see after that when needed.  Terms:
;;   - entity = jack or element
;;   - element: midi <-> audio conversion (1 or more)
;;   - jack: midi source / sink  (identified by JackID)
;;   - embedded / external
;;   - cable number linked to embedded midi jack
;;   - terminal
;; Entities can be wired together through I/O pins
;; There are specific USB requests:
;;   In principle, all requests are optional. If a USB-MIDI function
;;   does not support a certain request, it must indicate this by
;;   stalling the control pipe when that request is issued to the
;;   function. However, if a certain Set request is supported, the
;;   associated Get request must also be supported.
;; Endpoint descriptors have 2 extra bytes.



(define (MIDI-OUT-Jack-Descriptor
         #:bJackID   [jid 1]
         #:bJackType [jt 1])
  (Descriptor
   (bLength (+ 2 7))  ;; 7 + 2 * nr_inputs
   (bDescriptorType    #x24)     ;; CS_INTERFACE
   (bDescriptorSubtype #x03)     ;; MIDI_OUT_JACK
   (bJackType          jt)       ;; #x01:EMBEDDED / #0x02:EXTERNAL
   (bJackID            jid)      ;; ID of this Jack
   (bNrInputPins       #x01)     ;; Number of Input Pins of this Jack  (at least one?)
   ;; For each pin:
   (BaSourceID         #x02)     ;; ID of the Entity to which this pin is connected
   (BaSourcePin        #x01)     ;; Output Pin numbere of the Entity to which this Input Pin is connected
   (iJack              #x00)))    ;; Unused

(define (MIDI-IN-Jack-Descriptor
         #:bJackID   [jid 1])
  (Descriptor
   (bLength 6)
   (bDescriptorType    #x24)     ;; CS_INTERFACE
   (bDescriptorSubtype #x02)     ;; MIDI_IN_JACK
   (bJackType          #x02)     ;; EXTERNAL
   (bJackID            jid)     ;; ID of this Jack
   (iJack              #x00)))   ;; unused

(define (ElementDescriptor
         #:bmElementCaps    [ec #x01] ;; D0: CUSTOM UNDEFINED
         #:bInTerminalLink  [bi #x00] ;; no link
         #:bOutTerminalLink [bo #x00] ;; no link
         #:bElementID       [eid 1]
         )
  (Descriptor
   (bLength 11)
   (bDescriptorType #x24)     ;; CS_INTERFACE
   (bDescriptorSubtype #x04)  ;; ELEMENT
   (bElementID eid)           ;; Midi OUT jack
   (bNrInputPins 0)
   (bNrOutputPins 0)
   (bInTerminalLink bi)
   (bOutTerminalLink bo)
   (bElCapSize 1)
   (bmElementCaps ec)
   (iElement 0)))
   


(define (InterfaceDescriptorMIDI
         #:bInterfaceNumber [in 0])


  ;; INTERFACE: communication
  (InterfaceDescriptor
   #:bInterfaceNumber   in
   #:bInterfaceClass    #x01  ;; AUDIO
   #:bInterfaceSubClass #x03  ;; MIDISTREAMING
   #:bNumEndpoints      1)
  
  ;; Class-specific MS Interface Header Descriptor
  (TotalLength (nb_bytes_midi)
   (Descriptor 
    (bLength 7)
    (bDescriptorType    #x24)     ;; CS_INTERFACE
    (bDescriptorSubtype #x01)     ;; MS_HEADER
    (bcdMSC #x0100)               ;; release number
    (wTotalLength nb_bytes_midi)) ;; class-specific MIDIStreaming interface desc
   
   (MIDI-IN-Jack-Descriptor
    #:bJackID   #x02) ;; ???
   (MIDI-OUT-Jack-Descriptor
    #:bJackID   #x03  ;; ???
    #:bJackType #x01) ;; EMBEDDED
   (MIDI-OUT-Jack-Descriptor
    #:bJackID   #x04  ;; ???
    #:bJackType #x01) ;; EMBEDDED
   
   (InterruptEndpoint
    #:bEndpointAddress #x82)
   
   (BulkEndpoint #:bEndpointAddress #x01) ;; OUT1
   (BulkEndpoint #:bEndpointAddress #x81) ;; IN1
   ))


     

      
  

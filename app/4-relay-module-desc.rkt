#lang racket/base
(require staapl/pic18/usb-desc)
(provide (all-defined-out))

;; Define descriptors in quoted using 'scheme in dip28kit.fm
(define-values (device configs strings)
  (usb-device
   (DeviceDescriptor
    ;; Use serial number to refer back to dictionary.
    #:iSerialNumber "dip28kit.dict"
    )
   (configuration

    (InterfaceDescriptorCDC
     #:bInterfaceNumber 0
     #:bEndpointAddress #x82)

    (InterfaceDescriptorCDCdata
     #:bInterfaceNumber 1
     #:bEndpointAddressIN  #x81
     #:bEndpointAddressOUT #x01)

;    (InterfaceDescriptorAUDIOCONTROL
;     #:bInterfaceNumber 2)

;    (InterfaceDescriptorMIDI
;     #:bInterfaceNumber 3
;     #:bEndpointAddressIN  #x83
;     #:bEndpointAddressOUT #x03)
    )))


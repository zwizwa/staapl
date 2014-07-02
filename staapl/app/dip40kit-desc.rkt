#lang racket/base
(require staapl/pic18/usb-desc)
(provide (all-defined-out))

;; Define descriptors in quoted using 'scheme in dip40kit.fm
(define-values (device configs strings)
  (usb-device
   (DeviceDescriptor
    ;; Use serial number to refer back to dictionary.
    #:iSerialNumber "dip40kit.dict"
    )
   (configuration
    (InterfaceDescriptorCDC
     #:bInterfaceNumber 0
     #:bEndpointAddress #x82)
    (InterfaceDescriptorCDCdata
     #:bInterfaceNumber 1
     #:bEndpointAddressIN  #x81
     #:bEndpointAddressOUT #x01)
    (InterfaceDescriptorMIDI
     #:bInterfaceNumber 2
     #:bEndpointAddressIN  #x83
     #:bEndpointAddressOUT #x03)
    )))


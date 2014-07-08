#lang racket/base
(require staapl/pic18/usb-desc)
(provide (all-defined-out))

;; Define descriptors in quoted using 'scheme in dtc.fm
(define-values (device configs strings)
  (usb-device
   (DeviceDescriptor
    ;; Use serial number to refer back to dictionary.
    #:iSerialNumber "dtc.fm"
    )
   (configuration

    (InterfaceDescriptorCDC
     #:bInterfaceNumber 0
     #:bEndpointAddress #x82
     #:bSlaveInterfaces '(1))
    (InterfaceDescriptorCDCdata
     #:bInterfaceNumber 1
     #:bEndpointAddressIN  #x81
     #:bEndpointAddressOUT #x01)

    (InterfaceDescriptorCDC
     #:bInterfaceNumber 2
     #:bEndpointAddress #x84
     #:bSlaveInterfaces '(3))
    (InterfaceDescriptorCDCdata
     #:bInterfaceNumber 3
     #:bEndpointAddressIN  #x83
     #:bEndpointAddressOUT #x03)
    

    )))


#lang racket/base
(require staapl/pic18/usb-desc)
(provide (all-defined-out))

;; Define descriptors in quoted using 'scheme in dip40kit.fm
(define-values (device configs strings)
  (usb-device
   (DeviceDescriptor
    #:bNumConfigurations 2
    #:iSerialNumber "dip40kit-desc.rkt")
   (ConfigurationDescriptorCDC)
   (ConfigurationDescriptorMIDI
    #:bConfigurationValue 2)
   ))

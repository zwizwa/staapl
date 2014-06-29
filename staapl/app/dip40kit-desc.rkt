#lang racket/base
(require staapl/pic18/usb-comp
         staapl/pic18/usb-desc
         racket/pretty)
(provide (all-defined-out))

;; Provide interface between USB descriptor constructors written in
;; Scheme, and the macro language.

(define-values
  (desc-device
   desc-config
   desc-strings)
  (DescriptorContext
   (let ((descriptors
          (list
           (prefix-length (chunk (DeviceDescriptor #:iSerialNumber "dip40kit-desc.rkt")))
           (prefix-length (chunk (ConfigurationDescriptorCDC)))
           (string-descriptors)
           ;;(map prefix-length (string-descriptors))
           )))
     (apply values descriptors)
     )))



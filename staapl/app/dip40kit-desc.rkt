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
   desc-string0
   desc-string1
   desc-string2
   desc-string3
   ;;desc-string
  )
  (DescriptorContext
   (let ((descriptors
          (list
           (chunk (DeviceDescriptor #:iSerialNumber "dip40kit-desc.rkt"))
           (chunk (ConfigurationDescriptorCDC))
           (string-descriptor 0)
           (string-descriptor 1)
           (string-descriptor 2)
           (string-descriptor 3))))
     ;; (pretty-print descriptors)
     (apply values
            (map prefix-length descriptors))
     )))



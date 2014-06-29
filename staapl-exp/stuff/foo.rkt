#lang racket/base
(require racket/foreign)

(unsafe!)


(define-cstruct _usb-device-descriptor
  ([usbMajor           _uint8]
   [usbMinor           _uint8]))

(define-cstruct _usb-device
  ([next         _usb-device-pointer/null]
   [prev         _usb-device-pointer/null]
   [descriptor   _usb-device-descriptor]
   ))

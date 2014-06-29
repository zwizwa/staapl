#lang racket/base

(provide (all-defined-out))
;; USB constants definitions, verbatim from USB 1.1 spec document.

;; Defined separately here to use in both host and client code.

;; Table 9.2. Format of setup data. p. 183
;; Data transfer direction
(define USB_ENDPOINT_OUT     #x00)  ;; Host-to-device
(define USB_ENDPOINT_IN      #x80)  ;; Device-to-host
;; Type
(define USB_TYPE_STANDARD #x00)
(define USB_TYPE_CLASS    #x20)
(define USB_TYPE_VENDOR   #x40)
;; Recipient
(define USB_RECIP_DEVICE	#x00)
(define USB_RECIP_INTERFACE	#x01)
(define USB_RECIP_ENDPOINT	#x02)

;; All the constants use the original names from the USB 1.1 spec.

;; Table 9.4 Standard request codes p.187
(define USB_REQ_GET_STATUS		#x00)
(define USB_REQ_CLEAR_FEATURE		#x01)
;; #x02 is reservd
(define USB_REQ_SET_FEATURE		#x03)
;; #x04 is reserved
(define USB_REQ_SET_ADDRESS		#x05)
(define USB_REQ_GET_DESCRIPTOR		#x06)
(define USB_REQ_SET_DESCRIPTOR		#x07)
(define USB_REQ_GET_CONFIGURATION	#x08)
(define USB_REQ_SET_CONFIGURATION	#x09)
(define USB_REQ_GET_INTERFACE		#x0A)
(define USB_REQ_SET_INTERFACE		#x0B)
(define USB_REQ_SYNCH_FRAME		#x0C)

;; Table 9.5 Descriptor types p.187
(define USB_DT_DEVICE			#x01)
(define USB_DT_CONFIGURATION		#x02)
(define USB_DT_STRING			#x03)
(define USB_DT_INTERFACE		#x04)
(define USB_DT_ENDPOINT		        #x05)

(define USB_DT_HID			#x21)
(define USB_DT_REPORT			#x22)
(define USB_DT_PHYSICAL			#x23)
(define USB_DT_HUB			#x29)


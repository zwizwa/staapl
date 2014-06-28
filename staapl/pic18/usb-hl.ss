#lang racket/base
;; High-level specification of USB descriptor structures.

;; Compilation buffer
(define *rbytes* (make-parameter '()))
(define (bytes) (reverse (*rbytes*)))
(define (push-byte x) (*rbytes* (cons x (*rbytes*))))
(define (push-word nb-bytes value)
  (when (> nb-bytes 0)
    (push-byte (bitwise-and value #xFF))
    (push-word (sub1 nb-bytes) (arithmetic-shift value -8))))
(define (bytes-chunk thunk)
  (parameterize ((*rbytes* '()))
    (thunk)
    (reverse (*rbytes*))))
(define (push-bytes bs)
  (when (not (null? bs))
    (push-byte (car bs))
    (push-bytes (cdr bs))))
                 

;; String buffer
(define *rstrings* (make-parameter '()))
(define (push-string s)
  (let ((str (*rstrings*)))
    (push-byte (length str))
    (*rstrings* (cons s str))))

;; Types
(define (byte arg) (push-word 1 arg))
(define (word arg) (push-word 2 arg))
(define (istring arg) (push-string arg))


;; Structure
(define (_descriptor thunk)
  (let* ((chunk (bytes-chunk thunk)))
    (let ((l1 (length chunk))
          (l2 (list-ref chunk 0)))
      (when (not (= l1 l2))
        (raise `(descriptor-size error ,l1 ,l2)))
      (push-bytes chunk))))
  
(define-syntax-rule (descriptor forms ...)
  (_descriptor (lambda () forms ...)))



(define-syntax-rule (define-descriptor-field (name typ) ...)
  (begin (define name typ) ...))

(define-descriptor-field

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


(define (mBulkEndpoint addr)
  (descriptor
   (bLength 7);; Length is verified
   (bDescriptorType 5)
   (bEndpointAddress addr)
   (bmAttributes #x02) ;; BULK
   (wMaxPacketSize 64)
   (bInterval 0)))


(define (mInterruptEndpoint addr)
  (descriptor
   (bLength 7);; Length is verified
   (bDescriptorType 5)
   (bEndpointAddress addr)
   (bmAttributes #x03) ;; BULK
   (wMaxPacketSize 64)
   (bInterval 10)))
          
           
(define (mDeviceDescriptor
         #:bMaxPacketSize [mps 64]
         #:idVendor       [iv #x05F9]
         #:idProduct      [ip #xFFF0]
         #:iManufacturer  [m "Zwizwa"]
         #:iProduct       [p "Staapl"]
         #:iSerialNumber  [s "um0"]
         )
  (descriptor
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
  
  
  

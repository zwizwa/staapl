#lang scheme/base

;; FFI bindings for libusb.

;; This sticks to the origininal usb.h struct and field naming.  The
;; usb.h struct names are prefixed with "struct:" to avoid name
;; clashes with functions, which are imported verbatim.
;;
;; Note that `define-cstruct' adds dashes for field
;; accessors, which disambiguates things like:
;;
;;   struct:usb_device_descriptor   a struct syntax id
;;
;;   struct:usb_device-descriptor   a field accessor function
;;
;; While this naming scheme is a bit ugly, I find it easier to see
;; what's going on in the absence of any documentation or proper
;; high-level abstraction.  I.e. if you see struct:usb_foo-bar or
;; usb_foo_bar, you know to go look in libusb.  If you see usb-foo-bar
;; you know it's a higher level function defined in libusb.ss
;;
;;
;; Adapted from code posted by Jakub Piotr Cłapa
;; http://list.cs.brown.edu/pipermail/plt-scheme/2007-March/016671.html


(require
 (lib "foreign.ss")
 (lib "etc.ss")
 "usbconst.ss"
 )

(unsafe!)


(provide

 ;; High-level functions defined in libusb.ss
 usb-device-list
 usb-device-product

 ;; C functions from libusb (see libusb.h)
 usb_init
 usb_find_busses
 usb_find_devices
 usb_open
 usb_get_driver_np
 usb_detach_kernel_driver_np
 usb_set_configuration
 usb_claim_interface
 usb_release_interface
 usb_interrupt_write
 usb_interrupt_read
 
 ;; get-vendor-id
 ;; get-product-id
 ;; ids-filter
 ;; string-ids-filter
 
)

;; PLATFORM

(define (find-ffi-lib . _libs)
  (let try ((libs _libs))
    (if (null? libs)
        (error 'find-ffi-lib: "Can't find libusb, tried ~s\n" _libs)
        (let ((lib (car libs)))
          ; (printf "trying ~a\n" lib)
          (or
           (and (file-exists? lib)
                (ffi-lib lib))
           (try (cdr libs)))))))

(define libusb 
  (case (system-type)
    [(macosx)
     (ffi-lib "/System/Libraries/IOKit.framework/IOKit")
     (ffi-lib "/opt/local/lib/libusb") ]
    [(unix)
     ;; FIXME: This is a generic one + Debian-specific.  Is there a better way?"
     (find-ffi-lib
      "/lib/libusb-0.1.so"
      "/lib/libusb-0.1.so.4"
      "/lib/x86_64-linux-gnu/libusb-0.1.so.4"
      "/lib/i386-linux-gnu/libusb-0.1.so.4"
      )]
    [(windows)
     (ffi-lib "libusb0")]))

(define usb-max-path-len
  (case (system-type)
    [(unix) (+ 1 4096)]
    [(macosx) 1024]
    [(windows) 512]))


;; TYPES

;; Beware that usb.h has a mix of byte-packed and default-packed
;; structs.  Inheritance is not used as it seems to have some problems
;; Manual: "id: structure-type information compatible with struct-out
;; or match (but not define-struct); currently, this information is
;; correct only when no super-id is specified.")

;; * PACKED *

(define-cstruct _struct:usb_descriptor_header
  ([bLength          _uint8]
   [bDescriptorType  _uint8])
  #:alignment 1)


(define-cstruct _struct:usb_hid_descriptor
  ([bLength          _uint8]
   [bDescriptorType  _uint8]
   [bcdHID           _uint16]
   [bCountryCode     _uint8]
   [bNumDescriptors  _uint8])
  #:alignment 1)

(define-cstruct _struct:usb_device_descriptor
  ([bLength            _uint8]
   [bDescriptorType    _uint8]
   [bcdUSB             _uint16]
   [bDeviceClass       _uint8]
   [bDeviceSubClass    _uint8]
   [bDeviceProtocol    _uint8]
   [bMaxPacketSize0    _uint8]
   [idVendor           _uint16]
   [idProduct          _uint16]
   [bcdDevice          _uint16]
   [iManufacturer      _uint8]
   [iProduct           _uint8]
   [iSerialNumber      _uint8]
   [bNumConfigurations _uint8])
  #:alignment 1)


;; * NOT PACKED *
(define-cstruct _struct:usb_endpoint_descriptor
  ([bLength          _uint8]
   [bDescriptorType  _uint8]
   [bEndpointAddress _uint8]
   [bmAttributes     _uint8]
   [wMaxPacketSize   _uint16]
   [bInterval        _uint8]
   [bRefresh         _uint8]
   [bSynchAddress    _uint8]
   [extra            _pointer]
   [extralen         _int]
   ))

(define-cstruct _struct:usb_interface_descriptor
  ([bLength           _uint8]
   [bDescriptorType   _uint8]
   [bInterfaceNumber  _uint8]
   [bAlternateSetting _uint8]
   ;; ... 
   ))

(define-cstruct _struct:usb_config_descriptor
  ([bLength          _uint8]
   [bDescriptorType  _uint8]
   ;; ...
   ))




;; path
(define (make-carray-type _x n)
  (make-cstruct-type
   (build-list n (lambda (i) _x))))
(define (cptr->bytes0 ptr max)
  (define (strlen b [n 0])
    (if (zero? (bytes-ref b n)) n
        (strlen b (add1 n))))
  (let ((b0 (make-sized-byte-string ptr max)))
    (subbytes b0 0 (strlen b0))))
(define (make-cmaxstring-type n)
  (make-ctype
   (make-carray-type _byte n)
   #f
   (lambda (ptr)
     (bytes->string/utf-8 
      (cptr->bytes0 ptr n)))))

(define _path-type (make-cmaxstring-type usb-max-path-len))




;; string

;; A 'buffer' is a byte string.

;; try to distinguish: ptr, byte-buffer, highlevel types.

;; Use a raw byte buffer
(define (cptr->descriptor-buffer ptr)
  (make-sized-byte-string ptr (- (ptr-ref ptr _uint8) 2)))

(define (string-descriptor-buffer->string buffer)
  (let ([length (- (bytes-ref buffer 0) 2)]
        [type (bytes-ref buffer 1)])
    (unless (eq? type 3)
      (error 'string-descriptor "not a string descriptor"))
    (unless (>= (bytes-length buffer) length)
      (error 'string-descriptor "string longer than the buffer"))
    (let*-values ([(buffer) (subbytes buffer 2 (+ length 2))]
                  [(converter) (bytes-open-converter "UTF-16LE" "UTF-8")]
                  [(result length status) (bytes-convert converter buffer)])
      (bytes-close-converter converter)
      (bytes->string/utf-8 result))))


;; Not used?  Gives a contract violation.
;(define-cpointer-type
;  _struct:usb_string_descriptor
;  _struct:usb_descriptor_header  ;; ptr-type
;  #f                             ;; scheme-to-c
;  (lambda (ptr)                  ;; c-to-scheme
;    (string-descriptor-buffer->string
;     (cptr->descriptor-buffer ptr))))




(define _struct:usb_bus/patch _pointer)
(define-cstruct _struct:usb_device
  ([next         _struct:usb_device-pointer/null]
   [prev         _struct:usb_device-pointer/null]
   [filename     _path-type]
   [bus          _struct:usb_bus/patch]
   [descriptor   _struct:usb_device_descriptor]
   [config       (_cpointer _struct:usb_config_descriptor)]
   [dev          _pointer]
   [devnum       _uint8]
   [num_children _uint8]
   [children     (_cpointer _struct:usb_device-pointer)]))

(define-cstruct _struct:usb_bus
  ([next         _struct:usb_bus-pointer/null]
   [prev         _struct:usb_bus-pointer/null]
   [dirname      _path-type]
   [devices      _struct:usb_device-pointer/null]
   [location     _uint32]
   [root-dev     _struct:usb_device-pointer/null]))

(set! _struct:usb_bus/patch _struct:usb_bus-pointer)

(define _usb-class
  (_enum '(per-interface audio comm hid printer mass-storage hub data
                         vendor-spec = #xff)))
(define _usb-request-type _uint)
(define _usb-request      _uint)


(define-cpointer-type _usb-dev-handle)


;; FFI FUNCTIONS

(define-syntax defusb
  (syntax-rules ()
    [(_ name type ...)
     (define name
       (get-ffi-obj 'name ;; (regexp-replaces 'name '((#rx"-" "_")))
                    libusb (_fun type ...)))]))



(defusb usb_strerror -> (message : _bytes)
  -> (bytes->string/latin-1 message))


(defusb usb_init -> _void)
(defusb usb_find_busses -> _int)
(defusb usb_find_devices -> _int)

(defusb usb_get_busses -> _struct:usb_bus-pointer)

(defusb usb_open   _struct:usb_device-pointer -> _usb-dev-handle)
(defusb usb_device _usb-dev-handle            -> _struct:usb_device)
(defusb usb_close  _usb-dev-handle            -> _int)


(define (usb-check retv)
  (when (< retv 0)
    (error (usb_strerror)))
  retv)

;; Send out a control message and wait for a raw result.
(defusb usb_control_msg (dev requesttype request value index buflen timeout) ::
  (dev : _usb-dev-handle)
  (requesttype : _usb-request-type)
  (request : _usb-request)
  (value : _int)
  (index : _int)
  (buffer : (_bytes o buflen))
  (buflen : _int)
  (timeout : _int)
  -> (recvlen : _int)
  -> (subbytes buffer 0 (usb-check recvlen)))

(defusb usb_set_configuration _usb-dev-handle _uint
  -> (retv : _int)
  -> (void (usb-check retv)))

(defusb usb_claim_interface _usb-dev-handle _uint
  -> (retv : _int)
  -> (void (usb-check retv)))
(defusb usb_release_interface _usb-dev-handle _uint
  -> (retv : _int)
  -> (void (usb-check retv)))

(defusb usb_interrupt_write _usb-dev-handle _uint _bytes _int _int
  -> (retv : _int)
  -> (void (usb-check retv)))

(defusb usb_interrupt_read _usb-dev-handle _uint _bytes _int _int
  -> (retv : _int)
  -> (usb-check retv))

;; FIXME: linux specific
(defusb usb_get_driver_np _usb-dev-handle _int _bytes _int
  -> (retv : _int)
  -> (usb-check retv))
(defusb usb_detach_kernel_driver_np _usb-dev-handle _int
  -> (retv : _int)
  -> (usb-check retv))


;; HIGHLEVEL FUNCTIONS


;; Control message send.
(define (usb-control dev requesttype request value index [buflen 255] [timeout 5000])
  (let ((buf (usb_control_msg dev requesttype request value index buflen timeout)))
    buf))
  



;; Map over linked structs.
(define (usb-map-list first-elem next-fun map-fun)
  (let loop ([elem first-elem])
    (if elem
        (cons (map-fun elem) (loop (next-fun elem)))
        '())))
(define (usb-map-busses map-fun)
  (usb-map-list (usb_get_busses) struct:usb_bus-next map-fun))
(define (usb-map-devices device map-fun)
  (usb-map-list device struct:usb_device-next map-fun))
(define (usb-map-all-devices [map-fun (lambda (x) x)])
  (apply append
         (usb-map-busses
          (lambda (bus)
            (usb-map-devices (struct:usb_bus-devices bus) map-fun)))))
(define (ids-filter vendor-id product-id)
  (lambda (device)
    (if (and
         (eq? (get-vendor-id device) vendor-id)
         (eq? (get-product-id device) product-id))
        device
        #f)))

(define (string-ids-filter manufacturer product)
  (lambda (device)
    (if (and
         (equal? (usb-device-manufacturer device) manufacturer)
         (equal? (usb-device-product device) product))
        device
        #f)))



;; Device meta data query.
(define (get-vendor-id device)  (struct:usb_device_descriptor-idVendor  (struct:usb_device-descriptor device)))
(define (get-product-id device) (struct:usb_device_descriptor-idProduct (struct:usb_device-descriptor device)))

;; Wrappers around Standard Device Requests (USB 1.1 spec p. 186)
;; (define get-descriptor (make-request 'endpoint-in 'get-descriptor
(define (type/index type index)
  (+ index (arithmetic-shift type 8)))

(define (get-descriptor-buffer device type index [langid 0])
  (usb-control device
               USB_ENDPOINT_IN
               USB_REQ_GET_DESCRIPTOR
               (type/index type index)
               langid))

(define (usb-get-string device index langid)
  (string-descriptor-buffer->string
   (get-descriptor-buffer device USB_DT_STRING index langid)))

;; Need a connectuib to get a string.
(define (string-getter id)
  (lambda (device)
    (let* ([handle (usb_open device)]
           [result (usb-get-string handle
                                   (id (struct:usb_device-descriptor device))
                                   0)])
      (usb_close handle)
      result)))
(define (value-getter id)
  (lambda (device)
    (id (struct:usb_device-descriptor device))))

;; High level query

(define usb-device-manufacturer  (string-getter struct:usb_device_descriptor-iManufacturer))
(define usb-device-product       (string-getter struct:usb_device_descriptor-iProduct))
(define usb-device-serial-number (string-getter struct:usb_device_descriptor-iSerialNumber))

(define usb-device-num-configurations (value-getter struct:usb_device_descriptor-bNumConfigurations))


(define (usb-device-list [vendor-id #f]
                         [product-id #f])
  (let ((devs (usb-map-all-devices)))
    (if vendor-id
        (filter (ids-filter vendor-id product-id) devs)
        devs)))


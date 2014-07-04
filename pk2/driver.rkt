#lang racket/base
; (provide (all-defined-out))
(provide pk2  ;; from 
         pk2-close
         pk2-boot
         )

;; Driver hub, glues together the parts to get a working PK2 USB
;; connection and device database.

(require
 "util.rkt"
 "libusb.rkt"       ;; FFI + tools
 "usbconst.rkt"     ;; standard USB bits
 "pk2const.rkt"     ;; PICkit2 bits
 "pk2script.rkt"    ;; command and scripting languages
 "interpreter.rkt"  ;; interpreter (for send/receive dep inject)
 "device-file.rkt"  ;; Reader for Microchip's programming scripts
 racket/pretty)


;(define (reboot)
;  ; (RESET)
;  (sleep 1)
;  (boot))


;; *** TRANSPORT DRIVER INTERFACE ***

;; (define-struct pk2-instance (handle open close send receive))

(define (pk2-open)        (open-usb))
(define (pk2-close)       (close-usb))
(define (pk2-send . a)    (apply usb_interrupt_write a))
(define (pk2-receive . a) (apply usb_interrupt_read a))

;; Current PK2 device
(define pk2-handle (make-parameter #f))


;; *** PROTOCOL DRIVER ***

;; Patch interpreter to use current.
;; No device -> default to debug mode.
(define (if-handle param fn)
  (let ((debug-fn (param)))
    (param
     (lambda args
       (apply (if (pk2-handle) fn debug-fn) args)))))

(if-handle interpreter-snd (lambda (bytes) (send-packet (pk2-handle) bytes)))
(if-handle interpreter-rcv (lambda ()      (receive-packet (pk2-handle))))

;; Get all PK2s
(define (pickit2-list-usb) (usb-device-list #x04d8 #x0033))


;; Open/Close a particular one



;; Send/Receive
(define endpoint-in  #x81)
(define endpoint-out #x01)
(define timeout 5000)


(define pk2-debug (make-parameter #f))

;; FIXME: Use either USB or USB_RPC protocol from usb_rpc.c
(define (send-packet dev buffer)
  (when (pk2-debug)
    (printf "SEND\n")
    (dump-list (bytes->list bytes)))
  (let ((l (bytes-length buffer)))
    (unless (= reqLen l)
      (error 'send-usb-wrong-size "~a" l))
    (pk2-send dev endpoint-out
              buffer l timeout)))
(define (receive-packet dev [bufsize reqLen])
  (let* ((b (make-bytes bufsize))
         (size
          (pk2-receive dev endpoint-in
                       b reqLen timeout))
         (bytes (subbytes b 0 size)))
    (when (pk2-debug)
      (printf "RECEIVE\n")
      (dump-list (bytes->list bytes)))
    bytes))



(define-syntax-rule (pk2 . a)
  (with-pk2 (lambda () (append . a))))

;; Command and Script exec
(define (with-pk2 thunk)
  (let* ((already (pk2-handle))
         (handle (or already (pk2-open))))
    (parameterize ((pk2-handle handle))
      (dynamic-wind
          READ_STATUS
          thunk
          (lambda ()
            (unless already
              (pk2-close (pk2-handle))))))))

;; Load database + connect
(define (pk2-reboot)
  (pk2-close)
  (pk2-boot))

(define (pk2-boot)
  (let ((datfile "/usr/local/bin/PK2DeviceFile.dat"))
    (printf "datfile:  ~a\n" datfile)
    (pk2-handle (pk2-open))
    (load-device-file datfile)
    ; (set! pk2-boot reboot)
    ))


;; *** TRANSPORT DRIVER RPC ***
;; Abstract RPC mechanism from usb_rpc.c





;; *** TRANSPORT DRIVER USB ***
;; use libusb on the host
(define (first-usb)
  (usb_init)
  (usb_find_busses)
  (usb_find_devices)
  (let ((pk2s (pickit2-list-usb)))
    (when (null? pk2s)
      (error 'no-pickit2-found))
    (car pk2s)))

(define (open-usb [dev (first-usb)])
  (let ((handle (usb_open dev)))
    ;; Detach if necessary.
    (with-handlers ((void void))
      (usb_get_driver_np handle 0 (make-bytes 31) 31)
      (usb_detach_kernel_driver_np handle 0))
    (usb_set_configuration handle 2)  ;; try vendor config (not HID!)
    (usb_claim_interface handle 0)
    (printf "iProduct: ~a\n"
            (usb-device-product dev))
    handle))

(define (close-usb [handle
                    (let ((h (pk2-handle)))
                      (pk2-handle #f) h)])
  (when handle
    ;; (display "Releasing PicKit2.\n")
    (usb_release_interface handle 0))
  ; (set! pk2-boot (lambda () (printf "trying to reopen PK2\n")))
  )
  



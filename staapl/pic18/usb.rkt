;; meta code for generating USB descriptors

;; USB is an object system with an assymetric client/server
;; architecture.  The host (client) sends request to the device
;; a.k.a. function (server).

;; Forth code implements:
;;  - low level PIC-specific USB controller driver
;;  - ISR to service USB requests
;;  - GET_DESCRIPTOR for Device / Configuration / String


;; DESCRIPTOR RECORDS

;; http://www.beyondlogic.org/usbnutshell/usb5.htm#DeviceDescriptors

;; This defines the form `define-usb-device', which is a small wrapper
;; around the binary structures used in USB descriptors.  It abstracts:

;; - string allocation
;; - descriptor size computation
;; - low/high words
;; - number of descriptors


;; The following forth words are defined, returning Flash addresses:
;;   device-descriptor        \ -- lo hi
;;   configuration-descriptor \ n -- lo hi
;;   string-descriptor        \ n -- lo hi


;; The CONFIGURATION descriptor contains configuration, interface and
;; endpoint descriptors: CIEEEIEEEIEEE in a single reply, so we
;; compile them in this sequence.

;; Note that the device descriptor has 2 records sharing the same
;; identifier, but with different type (idProduct and iProduct). the
;; latter one i'm renaming to iProductName.


;; links
;; http://www.beyondlogic.org/usbnutshell/
;; http://www.beyondlogic.org/usbnutshell/usb3.htm

#lang racket/base

(require
 "../pic18.rkt"
 "../pic18/string.rkt"
 "../pic18/route.rkt"
 (for-syntax
  racket/base
  "../tools.rkt"
  "usb-device.rkt"
  racket/pretty
  racket/match))

(provide define-usb-device)


;; *** GENERATOR ***

(define-syntax (define-usb-device stx)

  (define (interleave lst sym)
    (apply append (map (lambda (x) (list x sym)) lst)))
  ;; create a forth string from a list of bytes
  (define (fstring name lst)
    `(,name
      table->
      ,@(interleave
         (cons (length lst) lst) ;; not the same as add-length
         '|,|)))
  ;; create route + error code
  (define (route name lst error)
    `(,name
      ,(length lst) route/e
      ,@(interleave lst #'|.|)
      ,error))
  ;; generate a list of symbols with number postfix
  (define (namegen name n)
    (for/list ((i (in-range n)))
      (string->symbol (format "~a~a" name i))))

  ;; transform the data structure
  (define (generate-forth
           dev
           [stx #f] ;; for lexical context
           [string-descriptor (datum->syntax stx 'string-descriptor)]
           [device-descriptor (datum->syntax stx 'device-descriptor)]
           [configuration-descriptor (datum->syntax stx  'configuration-descriptor)]
           [config-error #'reset]
           [string-error #'reset])
    (match dev
           ((struct usb-device (desc confs strs))
            (let ((string-names (namegen 'string (length strs)))
                  (config-names (namegen 'config (length confs))))

              ;; Generate forth code.  It's flattened once on the top.  All
              ;; the subgens can return s-expressions.
              (let ((expr
                     (append
                      (list
                       (fstring device-descriptor desc)
                       (route string-descriptor string-names string-error)
                       (route configuration-descriptor config-names config-error))
                      (map fstring string-names strs)
                      (map fstring config-names confs))))
                ;; (pretty-print expr)
                #`(words #,@expr))))))


  (syntax-case stx ()
    ((_ dev)
     (generate-forth
      (usb-device-compile (syntax->datum #'dev))
      stx))))
                         



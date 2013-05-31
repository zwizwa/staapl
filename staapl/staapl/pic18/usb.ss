;; meta code for generating usb client/server code

;; usb is an object system with an assymetric client/server
;; architecture. the host (client) sends request to the device
;; (server).

;; it's implemented as a 'load-usb' function which just defines a
;; number of functions in a staapl forth file.


;; USB CONTROL FLOW, TRANSFER LAYER

;; this is best implemented directly in forth, since it's quite
;; straightforward.

;; simplified, the general idea is this:
;; - host sends SETUP/OUT packet
;; - device usb hardware buffers + sends ack
;; - usb hardware signals software
;; - software processes the OUT buffer, and synthesizes an IN (reply)
;; - usb hardware waits for host to poll reply, sends IN buffer

;; EP0 receives all control and status requests


;; DESCRIPTOR RECORDS

;; http://www.beyondlogic.org/usbnutshell/usb5.htm#DeviceDescriptors


;; The most trouble is in the descriptor data. It would be nice to
;; make a mini-language to generate most of the red tape, and some
;; small forth wrappers around the data structures.

;; only device, configuration and string will be requested during
;; enumeration. (others can follow?)

;; a configuration request will return configuration, interface and
;; endpoint descriptors: CIEEEIEEEIEEE in a single reply, so we
;; compile them in this sequence.



;; what can be automated?

;; - record length
;; - string references and management
;; - low/high words
;; - number of configurations

;; instead of gluing the type description to the name, it's probably
;; best to put a space there to avoid parsing. note that the device
;; descriptor has 2 records sharing the same identifier, but with
;; different type (idProduct and iProduct). the latter one i'm
;; renaming to iProductName.


;; i don't need to make it too general at first, just make sure the
;; general approach is not too hard to extend.

;; links
;; http://www.beyondlogic.org/usbnutshell/
;; http://www.beyondlogic.org/usbnutshell/usb3.htm

#lang scheme/base

(require
 "../pic18.ss"
 "../pic18/string.ss"
 "../pic18/route.ss"
 (for-syntax
  scheme/base
  "../tools.ss"
  "usb-device.ss"
  scheme/pretty
  scheme/match))

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
                         



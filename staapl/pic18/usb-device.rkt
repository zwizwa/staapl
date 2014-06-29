;; Parse a usb device descriptor format into a collection of binary
;; records that can be used to generate code for serving them to the
;; host.


#lang racket
(require "../tools.rkt"
         rnrs/bytevectors-6
         racket/match)
(provide (struct-out usb-device)
         usb-device-compile)

;; *** PARSER ***

;; descriptor layout
(define descr-device
  '(1
    (bcd USB)
    (b   DeviceClass)
    (b   DeviceSubClass)
    (b   DeviceProtocol)
    (b   MaxPacketSize)
    (id  Vendor)  
    (id  Product)
    (bcd Device)
    (i   Manufacturer)
    (i   ProductName) ;; not the original name sice it's already used
    (i   SerialNumber)
    (i   NumConfigurations)))
      
(define descr-endpoint
  '(5
    (b  EndpointAddress)
    (bm Attributes)
    (w  MaxPacketSize)
    (b  Interval)))

(define descr-interface
  '(4
    (b InterfaceNumber)
    (b AlternateSetting)
    (i NumEndpoints)
    (b InterfaceClass)
    (b InterfaceSubClass)
    (b InterfaceProtocol)
    (i Interface)))

(define descr-configuration
  '(2
    (w  TotalLength)
    (b  NumInterfaces)
    (b  ConfigurationValue)
    (i  Configuration)
    (bm Attributes)
    (b MaxPower)))


;; compiles all descriptors from a single .usb file
;; the result is a tagged list of numbers, which will be mapped to
;; forth code.


(define-struct usb-device (descriptor configurations strings))

(define (usb-device-compile device)
  
  (define string-stack '())
  (define configurations (void))
  (define device-descriptor (void))
  
  ;; add a string to the list, return its id
  (define (make-string s)
    (let ((id (length string-stack))
          (bytes (string->list s)))
      (push! string-stack `(,(+ 2 (length bytes))
                            3  
                            ,@bytes))
      `(,id)))

  (define (compile-device!)
    (make-parent/children-bundle
     (lambda (descriptor collector)
       (set! configurations collector)
       (set! device-descriptor descriptor))
     make-string
     descr-device
     device
     (lambda (c)
       (compile-configuration
        make-string c))))

  (compile-device!)
  (make-usb-device device-descriptor configurations
                   (reverse string-stack)))

;; the pattern is: compile a descriptor, and capture all the
;; underlying child descriptors. this goes both for interface and
;; configuration.

;; configuration contains several interfaces.
(define (compile-configuration make-string
                               configuration)
  
  ;; interface contains several endpoints
  (define (compile-interface interface)
    (make-parent/children make-string
                          descr-interface
                          interface
                          compile-endpoint))

  (let
      ((config
        (make-parent/children make-string
                              descr-configuration
                              configuration
                              compile-interface)))
    (let ((total
           (lo+hi
            (length config))))
      
      ;; patch total length
      `(,(car  config)        ;; type
        ,(cadr config)        ;; config descr length
        ,@total               
        ,@(cddddr config))))) ;; rest
  
  
          

  
;; parent descriptor followed by child descriptors. parent contains
;; a count of children, and compiled version is concatenated.
  
(define (make-parent/children-bundle
         concat make-string
         proto dict compile-child)
           
  (let ((collector #f))
    (let ((descriptor
           (make-descriptor
            proto
            `((i . ,make-string)
              (l . ,(lambda (lst)
                      (set! collector
                            (map compile-child lst))
                      `(,(length lst)))))
            dict)))
      
      (concat descriptor collector))))



(define (make-descriptor proto extended-types dict)

  (let ((typeid  (car proto))
        (spec    (map cadr (cdr proto))))
    (add-length
     `(,typeid
       ,@(expand-record
          (lambda (type val)
            (let ((type-map
                   (assoc type
                          (append extended-types
                                  base-types))))
              (if type-map
                  ((cdr type-map) val)
                  (error 'undefined-type "~a" type))))
          dict spec)))))
       

;; main compilation/expansion driver: create a list of bytes from a
;; list of symbols, a dictionary including type info and abstract
;; value a type mapper.
(define (expand-record map-type dict spec)
  (let ((_dict (t/n->n/t dict)))
    (foldr
     (lambda (kar kdr)
       (let ((record (assoc kar _dict))) ;; (type value)
         (if record
             (append (apply map-type (cdr record)) kdr)
             (error 'undefined-field "~a" kar))))
     '() spec)))





;; independent
(define (compile-endpoint e)
  (make-descriptor descr-endpoint '() e))

(define (concat-descriptors d c)
  `(,@d ,@(apply append (reverse c))))

(define (make-parent/children make-string
                              proto dict compile-child)
  (make-parent/children-bundle
   concat-descriptors
   make-string proto dict compile-child))


(define (add-length lst)
  (cons (+ 1 (length lst)) lst))




;; USB uses UTF16-LE
(define (string->list str)
  (bytes->list (string->utf16 str 'little)))

;; transform (type name value) to (name type value)
(define (t/n->n/t lst)
  (map
   (lambda (l)
     (apply (lambda (t n v) `(,n ,t ,v)) l))
   lst))

(define (mask-byte a) (bitwise-and a #xff))
(define (shift-byte a) (arithmetic-shift a -8))
  
(define (lo+hi a) (map mask-byte `(,a ,(shift-byte a))))
(define (lo a) (list (mask-byte a)))

(define (dummy d) '(-1))

;; extend this with a string and list mapper for normal operation
(define base-types
  `((b   . ,lo)
    (bcd . ,lo+hi)
    (id  . ,lo+hi)
    (w   . ,lo+hi)
    (bm  . ,lo)
    ;; debug: override these
    (i   . ,dummy)
    (l   . ,dummy)
    ))



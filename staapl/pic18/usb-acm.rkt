#lang scheme/base
(require "usb.ss")
(provide (all-defined-out))

;; USB descriptor for a serial device handled by the Linux USB Generic
;; serial driver (linux/drivers/usb/serial/generic.c)
;; See description linux/Documentation/usb/usb-serial.txt

;; The basic idea is that the serial port is mapped to a pair of bulk
;; IN/OUT endpoints, which is as simple as it gets.

(define-usb-device
 ((bcd USB             #x110)
  (b   DeviceClass     0) ;; Defined at Interface level
  (b   DeviceSubClass  0)
  (b   DeviceProtocol  0)
  (b   MaxPacketSize   64)
  (id  Vendor        #x05F9) ; 
  (id  Product       #xFFFF) ;  FIXME: Linux ignores all descriptors
  (bcd Device          0) ;; device release number
  (i   Manufacturer    "Zwizwa")
  (i   ProductName     "Staapl USB Console")  ;; iProduct
  (i   SerialNumber    "ABC123")
  (l   NumConfigurations
       
       (((i  Configuration  "Serial")
         (b  ConfigurationValue 1)   ;; configuration number
         (bm Attributes     #xA0)    ;; remote wakeup
         (b  MaxPower       #x32)    ;; 100 mA
	 (w  TotalLength    -1)      ;; conf + int + endp
         (l  NumInterfaces

             ;; Debug serial
             (((i Interface         "Debug Console")
               (b InterfaceNumber   0)
               (b InterfaceClass    #x02)  ;; CDC
               (b InterfaceSubClass #x00)  ;; 
               (b InterfaceProtocol #x00)  ;; DC
               (b AlternateSetting  0)     ;; DC
               (l NumEndpoints             ;; excluding EP0
                  (((bm Attributes #x02)         ;; BULK
                    (b  EndpointAddress #x81)    ;; IN 1
                    (w  MaxPacketSize 64)
                    (b  Interval 0))
                   ((bm Attributes #x02)         ;; BULK
                    (b  EndpointAddress #x01)    ;; OUT 1
                    (w  MaxPacketSize 64)
                    (b  Interval 0)))))

              ;; Application
              ;; ((i Interface         "MIDI")
              ;;  (b InterfaceNumber   1)
              ;;  (b InterfaceClass    1)     ;; Audio
              ;;  (b InterfaceSubClass 3)     ;; MIDI Streaming
              ;;  (b InterfaceProtocol 0)
              ;;  (b AlternateSetting  0)

              ;;  ;; MIDI descriptors?
               
              ;;  (l NumEndpoints             ;; excluding EP0
              ;;     (((bm Attributes #x02)         ;; BULK
              ;;       (b  EndpointAddress #x82)    ;; IN 2
              ;;       (w  MaxPacketSize 64)
              ;;       (b  Interval 0))
              ;;      ((bm Attributes #x02)         ;; BULK
              ;;       (b  EndpointAddress #x02)    ;; OUT 2
              ;;       (w  MaxPacketSize 64)
              ;;       (b  Interval 0)))))
              
              )))))))


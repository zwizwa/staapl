#lang racket/base

;; Simplified interface on top of libusb.ss bindings

(require
 "driver.rkt" ;; USB+DB device driver
 "cmd.rkt"    ;; useful highlevel PK2 commands
 "uart.rkt"   ;; PK2 uart support
 "icsp.rkt"   ;; PK2 ICSP 2-wire synchronous serial protocol support
 "prog.rkt"   ;; ICSP device programming
 )

(provide pk2-program
         pk2-boot
         pk2-close
         ;; pk2-run
         target-on
         target-off
         )



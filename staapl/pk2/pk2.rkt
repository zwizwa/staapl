#lang scheme/base

;; Simplified interface on top of libusb.ss bindings

(require
 "driver.ss" ;; USB+DB device driver
 "cmd.ss"    ;; useful highlevel PK2 commands
 "uart.ss"   ;; PK2 uart support
 "icsp.ss"   ;; PK2 ICSP 2-wire synchronous serial protocol support
 "prog.ss"   ;; ICSP device programming
 )

(provide pk2-program
         pk2-boot
         pk2-close
         ;; pk2-run
         target-on
         target-off
         )



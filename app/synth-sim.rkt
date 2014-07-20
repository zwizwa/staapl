#lang racket/base

;; Example of how to use the pic18 sim.
;; 2014-JUL-20

;; While the sim is not feature complete wrt. peripheral device
;; simulation, it seems useful enough to just run subroutines.

(require staapl/pic18/sim
         (file "synth.fm"))

(define (reload)
  (reg-defaults!)     ;; initializes registers and ram to a pseudo useful state
  (flash-from-code!)  ;; initialize flash from the compiler's code output
  (trace '()))        ;; reset tracing

(define (test0)
  (reload)
  (call-word 0))

(define (test1)
  (reload)
  (call-word target/init-midi-buf)
  (print-trace))

(define (test2)
  (reload)
  (call-word target/lo-isr)
  (print-trace))

 

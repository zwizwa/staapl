#lang racket/base

;; Example of how to use the pic18 sim.
;; 2014-JUL-20

;; While the sim is not feature complete wrt. peripheral device
;; simulation, it seems useful enough to run individual words / isrs.

(require staapl/pic18/sim
         staapl/pic18/sim-tools
         staapl/target/rep
         racket/dict
         (file "simtest.fm"))

(define (reload)
  (reg-defaults!)     ;; initializes registers and ram
  (flash-from-code!)  ;; initialize flash from the compiler's code output
  (trace '()))        ;; reset tracing


(define (var tw)     (target-word-address tw))
(define (vars . tws) (map var tws))

;; Simulate MIDI in + read
(define (test1)
  (reload)
  (trace print-trace-item) ;; use immediate trace instead of list
  (fsr-set! 2 (make-uninitialized))
  (call-word target/test1))

 

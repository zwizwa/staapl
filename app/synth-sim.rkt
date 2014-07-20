#lang racket/base

;; Example of how to use the pic18 sim.
;; 2014-JUL-20

;; While the sim is not feature complete wrt. peripheral device
;; simulation, it seems useful enough to run individual words / isrs.

(require staapl/pic18/sim
         staapl/pic18/sim-tools
         (file "synth.fm"))

(define (reload)
  (reg-defaults!)     ;; initializes registers and ram
  (flash-from-code!)  ;; initialize flash from the compiler's code output
  (trace '()))        ;; reset tracing

(define (test0)
  (reload)
  (call-word 0))

(define (test1)
  (reload)
  (call-word target/init-midi-buf)
  (print-trace))


(define inspect-addr (make-parameter '()))
(define (do-inspect tag addr)
  (when (member addr (inspect-addr))
    (trace! (list tag addr))))

(define (mem-inspect vec)
  (define (read addr)
    (do-inspect 'read addr)
    (vector-ref vec addr))
  (define (write addr v)
    (do-inspect 'write addr)
    (vector-set! vec addr v))
  (make-memory read write))

(define (test2)
  (reload)
  (ram (mem-inspect (ram)))        ;; Add memory inspector
  (inspect-addr '(#x38))
  (eusart-read (lambda () #xF8))   ;; emulate midi stream
  (call-word target/init-midi-buf) ;; initialize serial buffer pointers
  (RCIF #t)                        ;; set interrupt flag
  (call-word target/lo-isr)        ;; run interrupt routine (adds byte to buffer)
  (call-word target/midi>)         ;; read from buffer onto data stack
  (print-trace)
  (printf "WREG=~x\n" (wreg)))


 

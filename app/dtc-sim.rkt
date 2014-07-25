#lang racket/base

;; Example of how to use the pic18 sim.
;; 2014-JUL-20

;; While the sim is not feature complete wrt. peripheral device
;; simulation, it seems useful enough to run individual words / isrs.

(require staapl/pic18/sim
         staapl/pic18/sim-tools
         staapl/target/rep
         staapl/tools
         racket/dict
         racket/pretty
         racket/generator
         (file "dtc.fm")
         )

(define *out* '())
(define (reload)
  (reg-defaults!)     ;; initializes registers and ram
  (flash-from-code!)  ;; initialize flash from the compiler's code output
  (flash (flash-extend (flash) #x8000 (make-vector #x4000 #f)))
  (eusart-write (lambda (v) (push! *out* v)))
  ;; (trace '()) ;; reset tracing
  (trace print-trace-item) ;; use immediate trace instead of list
  )
  

(define *ram* (vector-memory (make-vector #x1000 (make-empty))))
(define (dump) (memory-dump *ram* #x200 #x300))

;; Note that on PIC the RCIF flag is cleared after reading the buffer.
;; In the sim this doesn't happen, so we set it just once.
(define (send-string line)
  (RCIF #t)
  (rcsta 0)
  (txsta 0)
  (TRMT #t)
  (eusart-read
   (sequence->generator
    (map char->integer
         (string->list line)))))


(define (test0)
  (reload)
  (ram *ram*)
  (trace print-trace-item) ;; use immediate trace instead of list
  (send-string "hello\r")
  (call-word target/serial>io)
  (call-word target/line-editor))


(define (test1)
  (reload)
  (call-word target/test1)
  (pretty-print (ds)))


;; (current-directory "/home/tom/staapl/app") (enter! (file "dtc-sim.rkt")) (test1)

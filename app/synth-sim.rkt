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
         (file "synth.fm"))

(define (reload)
  (reg-defaults!)     ;; initializes registers and ram
  (flash-from-code!)  ;; initialize flash from the compiler's code output
  (trace '()))        ;; reset tracing

;; Boot kernel
(define (test0)
  (reload)
  (call-word 0))

(define (test1)
  (reload)
  (call-word target/init-midi-buf)
  (print-trace))

(define (var tw)     (target-word-address tw))
(define (vars . tws) (map var tws))


;; Simulate MIDI in + read
(define (test2)

  (reload)
  (trace print-trace-item) ;; use immediate trace instead of list

  ;; keep an eye on some addresses
  (define addrs (vars target/midi-write
                      target/midi-read))
  (define (access-notify addr . a)
    (when (member addr addrs)
      (trace! (cons 'var (cons addr a)))))
  (ram (memory-inspect
        (ram)
        access-notify
        access-notify))
  
  (eusart-read (lambda () #xF8))   ;; emulate midi stream
  (call-word target/init-midi-buf) ;; initialize serial buffer pointers
  (RCIF #t)                        ;; set interrupt flag
  (call-word target/lo-isr)        ;; run interrupt routine (adds byte to buffer)
  (call-word target/midi>)         ;; read from buffer onto data stack
  ;; (print-trace)
  (printf "WREG=~x\n" (wreg)))




(define (read-once var)
  (lambda ()
    (when (not var)
      (error 'already-consumed "~s" var))
    (let ((v var))
      (set! var #f)
      v)))

(define (send-byte! byte [n 1])
  (for ((i (in-range n)))
    (eusart-read (read-once byte))
    (RCIF #t)
    (call-word target/lo-isr)))


(define empty (make-uninitialized))

(define *ram* (make-vector #x1000 empty))

(define (dump)
  (for ((p (in-hex-printer 0 3 2 16 (lambda _ ".")))
        (i (in-range #x200)))
    (p (vector-ref *ram* i))))

(define (test3 [n 1])
  (reload)
  (ram (vector-memory *ram*))
  (trace print-trace-item) ;; use immediate trace instead of list
  (call-word target/init-midi-buf) ;; initialize serial buffer pointers
  (send-byte! #xF8 n))

 

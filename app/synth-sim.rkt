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



(define *ram* (make-vector #x1000 (make-empty)))
(define (dump)
  (for ((p (in-hex-printer 0 3 2 16 (lambda _ ".")))
        (i (in-range #x200)))
    (p (vector-ref *ram* i))))


(define (send-bytes! bytes)
  (for ((b bytes))
    (let ((bs (list b)))
      (eusart-read (lambda () (pop! bs))) ;; one read per val
      (RCIF #t)
      (vector-set! (fsr) 2 empty)
      (pretty-print `(pre . ,(fsr)))
      (call-word target/lo-isr)
      (pretty-print `(post . ,(fsr)))
      )))

(define (test3 [bytes '(#x90 64 127
                             65 127)])
  (reload)
  (ram (vector-memory *ram*))
  (trace print-trace-item) ;; use immediate trace instead of list
  (call-word target/init-midi-buf) ;; initialize serial buffer pointers
  (send-bytes! bytes))
 
(define (test4)
  (test3)
  (call-word target/midi>m)
  (call-word target/midi>m))

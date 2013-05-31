#lang scheme/base

;; Provide the staapl comm interface.

;; This converts the byte-channel interface to delimited messages by
;; assuming a request/response structure.  It is possible though
;; inefficient to do without an output buffer.

(require
 scheme/pretty
 "util.ss"
 "pk2.ss"
 "pk2script.ss"
 "cmd.ss"
 "icsp.ss")

(provide
 pk2-stat
 pk2-poll
 pk2-reset
 pk2-close
 pk2-in
 pk2-out
 ;; pk2-reset-programmer
 pk2-on
 pk2-off
 pk2-fast
 pk2-slow
 )

(define (icsp-us-set! us)
  (printf "pk2: clock ~s us\n" us)
  (icsp-us us))

(define (pk2-fast) (icsp-us-set! 3))  ;; 40 MHz FOSC
(define (pk2-slow) (icsp-us-set! 15)) ;;  8 MHz FOSC

(define-syntax-rule (@ f . a)
  (begin (display 'f) (newline) (f . a)))

(define (pk2-off)
  (@ target-off))
(define (pk2-on)
  (@ target-on))
(define (pk2-reset [secs .3])
  (pk2-off)  (sleep secs)
  (pk2-on)   (sleep secs)
  (pk2-poll) (sleep secs)  ;; 2 polls seem to be needed to resync some times.
  (pk2-poll)
  )

;; This only works when PK2 is still working properly.
;; Use pk2-close ; pk2-boot instead.
;;(define (pk2-reset-programmer)
;;  (display "Resetting PICkit2.\n")
;;  (RESET)
;; (sleep 1))


  

(define (pk2-stat)
  (pretty-print `(status ,@(status)))
  (pretty-print `(voltages ,@(voltages)))
  )


;; Apparently, reading a single bit is not enough.  The sync seems to
;; get lost sometimes.  The _assumption_ is that it starts
;; interpreting the idle line as a string of zeros, which is a nop so
;; it recovers eventually.
(define (icsp-poll max [disp 50])
  (let loop ((n 1))
    (if (> n max)
        #f
        (if (= 1 (icsp-recv-bit))
            (begin
              ;; Erase retry display
              (when (> n disp)
                (printf "\r    \r"))
              #t)
            (begin
              ;; Update retry display
              (when (> n disp)
                (printf "\r~a " n))
              (loop (add1 n)))))))

(define (pk2-poll [retries 1000])
  ;; To see if it's there we need to send a clock pulse.
  (let ((there (icsp-poll retries)))
    ;; If there is no reply, the target is not listening.
    (if there
        (begin
          ;; If there is a reply we need to complete the sequence and send
          ;; something.  Use the ack command = 2 zero bytes followed by
          ;; receive.
          (icsp-recv-bit)
          (icsp-send '(0 0) #:handshake #f)
          (let ((reply
                 (icsp-recv-message)))
            (if (equal? reply '(0 0))
                there
                (error 'pk2-poll "~a" reply))))
        #f)))


(define in-buffer '())
(define out-buffer '())

(define (pk2-flush)
  (unless (null? out-buffer)
    (icsp-send-message (reverse out-buffer))
    (set! out-buffer '())))

(define (pk2-in)
  (if (null? in-buffer)
      (begin
        ;; Read flushes write.
        (pk2-flush)
        (set! in-buffer (icsp-recv-message))
        (pk2-in))
      (let ((byte (pop! in-buffer)))
        byte)))

;; This could use the size-tagging to send the message when it's fully
;; buffered, instead of waiting for the pk2-in command to flush.
(define (pk2-out byte)
  (push! out-buffer byte))


;; Mostly to verify the handshake, this sends a message and receives a
;; reply using only pk2-in and pk2-out.  If the protocol is respected
;; (proper size tags and request/reply structure) this is enough to
;; talk to the on-target interpreter.
(define (rpc . msg)
  (pk2-out (length msg))
  (for ((b msg))
    (pk2-out b))
  (let ((size (pk2-in)))
    (printf "size = ~s\n" size)
    (for ((n (in-range size)))
      (printf "~s: ~s\n" n (pk2-in)))))

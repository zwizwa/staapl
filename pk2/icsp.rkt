#lang racket/base
(require
 racket/match
 "util.rkt"
 "pk2script.rkt")

(provide (all-defined-out))

;; Low-level byte-packet data communication using the synchronous
;; serial ICSP PGC/PGD lines.  This is a marriage between the Staapl
;; monitor protocol and the programming primitives available in an
;; unmodified Microchip PICkit2 USB programmer, firmware V2.32.
;;
;; On top of the straightforward synchronous serial byte in/out
;; primitives of the PK2 this requires some extra, ad-hoc handshake
;; protocol.  Operation is as follows:
;;
;; A Host starts handshake by sending out a clock pulse and reading
;;   the data line.  If target is there, it asserts a 1.  Otherwise
;;   the line is 0 due to PK2 weak pulldown.
;;
;; B If host did not receive a 1, it will retry step A.
;;
;; C Host sends out another clock pulse to allow the target to release
;;   the line, returning the bus to the host.
;;
;; D Host writes out command bytes:
;;   <address> <size> [<command> [<arg> ...]]
;;
;; E Host waits for handshake identical to step A and B.  Target will
;;   perform some computation, and when it is ready it will respond to
;;   the handshake.
;;
;; F Host provides clock for target to shift out a reply packet that
;;   has the same form as in step D.
;;
;; G Host clocks one more cycle for target to release the data line.
;;
;; H Next cycle restarts at A.
;;
;;
;;
;; Notes:
;;
;; * In D and F: target address is 0x00, host address is 0xFF.
;;
;; * In step D, the <size> = 0 case is a NOP that's used for
;;   recovering sync.  The target will ignore this message and wait
;;   for the next handshake clock pulse.
;;
;; * There seems to be a sync issue where PK2 delays the sync bit by
;;   one.  I don't think this is a Staapl bug as I get different
;;   behaviours for the same code.




(define (ICSP_OUT) (SET_ICSP_PINS 0))
(define (ICSP_IN)  (SET_ICSP_PINS 2))

;; Clock period is set to 15 as this seems to work for 8MHz internal
;; oscillator.  It can be set to 3us for 48MHz / 40MHz operation.
(define icsp-us (make-parameter 15))

(define icsp-debug (make-parameter #t))

;; Needs size-tagged message.
(define (icsp-send bytes
                   #:handshake handshake)
  (when (icsp-debug)
    (printf "icsp:send h:~a b:~a\n"
            handshake bytes))
  (when (> (length bytes) 26)
    (error 'buffer-overflow))

  ;; It seems to be necessary to do this in a separate transaction.
  ;; Assuming the device is there and sending it data creates
  ;; unrecoverable problems.
  (when handshake
    (let sync ()
      (when (zero? (icsp-recv-bit))
        (sync))))
  
  (apply EXECUTE_SCRIPT
         `(,(SET_ICSP_SPEED (icsp-us))
           ,@(if handshake
                 `(,(ICSP_IN)
                   ,(READ_BITS 1)) ;; 2nd handshake bit
                 '())
           ,(ICSP_OUT)
           ,@(for/list ((b bytes))
               (WRITE_BYTE_LITERAL b))
           ,(ICSP_IN))))

    

;; PK2 latches on falling edge.

;; Chunker: max 63 bytes at once.
(define (icsp-recv bytes
                   #:handshake (handshake #f)
                   #:ack (ack #f))
  (define max-bytes 63)
  (if (<= bytes max-bytes)
      (_icsp-recv bytes
                  #:handshake handshake
                  #:ack ack)
      (append
       (_icsp-recv max-bytes
                   #:handshake handshake
                   #:ack #f)
       (icsp-recv (- bytes max-bytes)
                  #:handshake #f
                  #:ack ack))))


(define (_icsp-recv bytes
                   #:handshake (handshake #f)
                   #:ack (ack #f))
  (define (log-msg reply)
    (format "icsp-recv: b:~a h:~a a:~a -> ~a"
            bytes handshake ack reply))

  ;; This would crash PK2, needing power cycle.
  (when (> bytes 63)
    (error 'icsp-recv-overflow "~s" bytes))
  
  (CLR_UPLOAD_BFR)
  (if (< bytes 1)
      (begin
        (when ack
          (EXECUTE_SCRIPT (READ_BITS 1)))
        '())
      (begin
        (apply EXECUTE_SCRIPT
               `(,(ICSP_IN)
                 ,(SET_ICSP_SPEED (icsp-us))
                 ,@(if handshake
                       ;; Handshake can be incorporated in recv packet
                       ;; for speedup of the most common case where
                       ;; the target is just sitting there waiting to
                       ;; send.  This adds an extra byte in front with
                       ;; handshake info.
                       ;;
                       ;; If the handshake byte's value is not 1 --
                       ;; the two handshake bits 1, 0 on the line --
                       ;; the rest of the buffer needs to be searched
                       ;; for a sync signal, and the clock needs to be
                       ;; re-aligned!
                       `(,(READ_BITS_BUFFER 2))
                       '())
                 ,(READ_BYTE_BUFFER) 
                 ,@(if (= 1 bytes)
                       '()
                       `(,(LOOP 1 (- bytes 1))))
                 ,@(if ack
                       ;; clock in and discard
                       `(,(READ_BITS 1))  
                       '())
                 ))
        (let ((reply (UPLOAD_DATA)))
          (let ((expect-size (+ bytes
                                (if handshake 1 0)))
                (real-size (length reply)))
                                
            (when (icsp-debug)
              (display (log-msg reply))
              (newline))
            (unless (= expect-size real-size)
              (printf "WARNING: Short UPLOAD_DATA: ~a (expected size ~a)\n"
                      reply expect-size))
            ;; This can return short count!
            reply)))))

;; Clock in a single bit.
(define (icsp-recv-bit)
  (CLR_UPLOAD_BFR)
  (EXECUTE_SCRIPT
   (ICSP_IN)
   (SET_ICSP_SPEED 255) ;; clock slowly
   (READ_BITS_BUFFER 1)
   ;; (ICSP_STATES_BUFFER) ;; for debug
   )
  (let ((data (UPLOAD_DATA)))
    (when (icsp-debug)
      (printf "icsp-recv-bit ~a\n" data))
    (car data)))


;; Write full message
(define (icsp-send-message m)
  (if #f
      (begin
        (icsp-handshake)  ;; handshake
        (icsp-send m))
      (icsp-send m #:handshake #t)))



;; Poll for handshake.

;; An ad-hoc protocol that works with using just the PGC/PGD lines and
;; the standard PK2 firmware.  Some extra bits are added to detect
;; target ready state, and hold the last bit without requiring
;; on-target timing.

(define (icsp-handshake)
  (let again ()
    ;; Poll until we have a 1 bit.
    (if (zero? (icsp-recv-bit))
        (begin
          ;; It seems OK to just poll as fast as we can without sleep.
          ;; The USB driver slows us down to the bus packet rate.  In
          ;; my current setup the delay between polling pulses is 3ms.
          ;; (sleep.1)
          (again))
        (icsp-recv-bit)))) ;; clock in second bit

;; Read full message with handshake.  This uses the Staapl message
;; byte protocol: <addr> <size> [<data>...]

;; Instead of waiting for ack, we just go ahead and read the 2 sync
;; bits followed by the 2 first message bytes.  If sync is incorrect
;; we realign it by reading extra bits.  This is in order to speed up
;; transfer, avoiding ping-pong across the USB bus delay.
(define (icsp-recv-message)

  ;; If necessary, this can re-align the bitstream.
  (define (resync handshake addr bytes)
    (let again ((bits (bior handshake
                      (bior (<<< addr  2)
                            (<<< bytes 10)))))
      (when (icsp-debug)
        (printf "icsp-recv-message: resync: ~b\n" bits))
      (cond
       ((= 1 (band bits 1))
        ;; Got sync, fish out header data and continue.
        (let ((addr  (band #xFF (>>> bits 2)))
              (bytes (band #xFF (>>> bits 10))))
          (recv-tail addr bytes)))
       ((zero? bits)
        ;; Idle line -> retry header receive.
        (recv-header))
       (else
        ;; One bit at a time.  Can be optimized by scanning for the
        ;; first bit, then read a bunc at a time.
        (again (bior (>>> bits 1)
                     (<<< (icsp-recv-bit) 17)))))))


  ;; Once header is parsed and synced, tail can be received.
  (define (recv-tail addr bytes)
    (let ((body (icsp-recv bytes #:ack #t)))
      (let ((reply (list* addr bytes body)))
        (when (icsp-debug)
          (printf "icsp-recv-message: ~a\n" reply))
        reply)))
    
  ;; Get the handshake byte followed by the 2-byte header.
  (define (recv-header)
    (let ((header (icsp-recv 2 #:handshake #t)))
      (match header
             ((list handshake addr bytes)
              (if (= 1 handshake)
                  ;; All OK.  This is the most common case for which we
                  ;; optimize.
                  (recv-tail addr bytes)
                  ;; If sync is off, the following will recover the sync
                  ;; by looking for the handshake bit.
                  (resync handshake addr bytes)))
             (else
              (error 'recv-header "malformed header: ~a" header)
              ))))
  (recv-header))


;; Probe the state of the input lines

;; use ICSP_STATES_BUFFER

(define (icsp-read-porta bit)
  ;; FIXME: Set both as input?
  (CLR_UPLOAD_BFR)
  (EXECUTE_SCRIPT (PEEK_SFR #x80))  ;; PORTA
  (band 1 (>>> (car (UPLOAD_DATA)) bit)))

(define (icsp-read-dat) (icsp-read-porta 2))
(define (icsp-read-clk) (icsp-read-porta 3))




;; (define (spi-send [speed 3]) ;; ok
;;   (EXECUTE_SCRIPT
;;    (SET_ICSP_SPEED (icsp-us))
;;    (SET_AUX 0) ;; AUX = out,0
;;    (ICSP_IN)   ;; PGC = out,0 ; PGD = in
;;    (SPI_WR_BYTE_LIT #x33)))

;; (define (i2c-send [speed 3])  ;; broken
;;   (EXECUTE_SCRIPT
;;    (SET_ICSP_PINS 4) ;; PGC = 1, out
;;    (SET_AUX 1)       ;; AUX = in
;;    (SET_ICSP_SPEED (icsp-us))
;;    (I2C_START)
;;    (I2C_WR_BYTE_LIT #x33)))





(define (icsp-recv-loop)
  (printf "Starting ICSP receive loop.\n")
  (let loop ()
    (let ((msg (icsp-recv-message)))
      (printf "~a ~a\n" (length msg) msg))
    (loop)))

;; Testing
; (pk2-boot)
; (icsp-recv-loop)


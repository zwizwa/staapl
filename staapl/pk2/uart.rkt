#lang scheme/base

;; ******** UART **********

(require
 "pk2script.ss"
 "util.ss"
 "cmd.ss")


(provide
 uart-read
 uart-try-read
 uart-write
 uart-start
 uart-stop
 uart-out)


;; Async channels used to throttle IO data: we need to poll the input
;; in time, and don't send too fast.

(require scheme/async-channel)
(define ->target (make-channel))        ;; Block sender if target can't keep up.
(define target-> (make-async-channel))  ;; We can't stop target, so we keep collecting.


(define uart-verbose (make-parameter #f))

;; Baud rate to PK2 delay period.
(define (baud rate)
  (let ((bv (->int (- 65536 (/ (- (/ 1. rate) 3e-6) 1.67e-7)))))
    (list (band bv #xFF)
          (band (>>> bv 8) #xFF))))


(define uart-stop void)

(define (uart-out bit)
  (EXECUTE_SCRIPT
   (SET_ICSP_PINS (bior #b0010
                        (<<< (band bit 1) 2)))))

(define (uart-start [rate 9600])

  ;; Time it takes to transmit a number of bytes.
  (define (bytes->ms bytes)
    (ceiling (* 1000 (/ (* 10 bytes) rate))))
  ;; Reader/Writer
  (define from #f)
  (define (from-target)
    (let ((bufr (UPLOAD_DATA)))
      ;; (printf ".")
      (when (uart-verbose)
        (unless (null? bufr) (printf "RX: ~a\n" bufr)))
      (if (null? bufr)
          (msleep (bytes->ms 1)) ;; poll once per byte time
          (for ((b bufr)) (async-channel-put target-> b)))
      (from-target)))
  (define to #f)
  (define (to-target)
    (let slurp ((l '()))
      (let ((b (channel-try-get ->target)))
        (if b
            (slurp (cons b l)) ;; try to get more
            (let ((bytes (reverse l)))
              (unless (null? bytes)
                (when (uart-verbose)
                  (printf "TX: ~a\n" bytes))
                  (void (apply DOWNLOAD_DATA bytes)))
              (msleep (bytes->ms (length bytes))) ;; wait for transmit
              (slurp (list (channel-get ->target)))))))) ;; block on first
  ;; Start/stop
  (define (stop)
    ;; (display "killing uart threads\n")
    (EXIT_UART_MODE)
    (kill-thread from)
    (kill-thread to)
    (set! uart-stop void))
  (define (start)
    (uart-stop)   ;; make sure no instance is running
    (READ_STATUS) ;; clear PK2 errors
    (apply ENTER_UART_MODE (baud rate))
    (set! from (thread from-target))
    (set! to   (thread to-target))
    (set! uart-stop stop))
  (start))

(define (uart-read)        (async-channel-get target->))
(define (uart-try-read)    (async-channel-try-get target->))
(define (uart-write . bytes) (for ((b bytes)) (channel-put ->target b)))


(define (test-read)
  (thread (lambda ()
            (let loop ()
              (display (uart-read))
              (newline)
              (loop)))))

(define (test-uart [rate 9600])
  (target-on)
  (uart-start rate)
  (let loop ()
    (display ".")
    (uart-write #x55)
    (sleep 1)
    (loop)))
      

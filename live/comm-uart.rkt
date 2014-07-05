#lang racket/base
(require
 "tethered.rkt"
 racket/system)

(provide
 comm-uart)


(define (comm-uart-timeout? ex) (eq? ex 'timeout))





;; serial port

(define stty
  (let ((fmt #f)
        (fmts
         '(("Linux"         . "stty -F ~a ~a raw min 1 -echo")
           ("CYGWIN_NT-5.1" . "stty -F ~a ~a min 1 -echo ixon -icanon pass8")
           ("windows"       . "mode ~a: baud=~a parity=n data=8 stop=1 xon=off dtr=off rts=off"))))
    (lambda (name baud)
      (unless fmt
        ;; Fixme: do autodetect using 'uname' or something..
        (set! fmt (cdr (assoc "Linux" fmts))))
      (unless baud ;; Probably means this is USB : make something up to keep stty happy
        (set! baud 115200)) 
      (when (and fmt name baud)
        (let ((cmd (format fmt name baud)))
          ;; (printf "config serial: ~a\n" cmd)
          (system cmd)))
      )))


(define (comm-uart name baud)

  (define (standard-serial-port)
    (define in  #f)
    (define out #f)

    (define (connect)
      (unless (and in out)
        (let-values
            (((i o)
              (open-input-output-file
               (d: "open-input-output-file ~s" name)
               #:exists 'append)))
          (file-stream-buffer-mode o 'none)
          (stty name baud)
          (set! in  i)
          (set! out o))))

    (comm-in
     (lambda ()
       ;; (printf "in:~s\n" in)
       (d: "uart-in ~x\n"
           ;; (read-byte-timeout i 3)
           (if (sync/timeout (comm-timeout) in)
               (read-byte in)
               (begin
                 ((comm-close))
                 (raise 'timeout)))
           )))
    (comm-out
     (lambda (b)
       ;; (printf "out:~s\n" out)
       (write-byte (d: "uart-out ~x\n" b) out)))
    (comm-close
     (lambda ()
       (when out (close-output-port (d: "close-output-port ~s" out)) (set! out #f))
       (when in  (close-input-port  (d: "close-output-port ~s" in))  (set! in  #f))
       ))
    (comm-reconnect
     (lambda ()
       ;;(printf "close ~s ~s\n" name baud)
       ((comm-close))
       ;;(printf "connect ~s ~s\n" name baud)
       (connect)))

    ;; (connect)
    
    )

  

  ;; FIXME: re-enable pk2 support through external console interface.
  
  ;; '(define (pk2-serial-port)
  ;;   (define (stop)
  ;;     (uart-stop) (target-off) (msleep 300))
  ;;   (define (start)
  ;;     (target-on)
  ;;     (uart-start baud)
  ;;     (comm-in uart-read)
  ;;     (comm-out uart-write)
  ;;     (comm-close
  ;;      (lambda () (stop) (pk2-close)))
  ;;     (comm-reset
  ;;      (lambda ()
  ;;        (printf "PK2: target cold reset.\n")
  ;;        (stop)     ;; don't pk2-close!
  ;;        (sleep 1)  ;; this ensures proper shutdown.  FIXME: fix thread sync!
  ;;        (start)))

  ;;     ;; Hack to start up communication.  For some reason, the first
  ;;     ;; ACK request doesn't seem to get through.  It does work when
  ;;     ;; proper PK2 shutdown is disabled (EXIT_UART_MODE), but then
  ;;     ;; programming doesn't work on next try.  This is probably a
  ;;     ;; glitch on the TX line...
  ;;     (begin
  ;;       (uart-write 1 6)
  ;;       (msleep 100)
  ;;       (let ((ret (uart-try-read)))
  ;;         ;; (printf "usart-start: ~a\n" ret)
  ;;         (unless ret
  ;;           (printf "uart-start hack\n"))))
  ;;     )
  ;;   (pk2-boot)
  ;;   (start))

  

  ((comm-close))


  ;; '(if (equal? "pk2" name)
  ;;      (pk2-serial-port)
  ;;      (standard-serial-port))
  
  (standard-serial-port)


  )



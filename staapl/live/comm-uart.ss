#lang scheme/base
(require
 "tethered.ss"
 scheme/system)

(provide
 comm-uart)




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
      (system (format fmt name baud)))))


(define (comm-uart name baud)

  (define (standard-serial-port)
    (define in #f)
    (define out #t)

    (define (connect)
      (let-values
          (((i o)
            (open-input-output-file name #:exists 'append)))
        (file-stream-buffer-mode o 'none)
        (stty name baud)
        (set! in  i)
        (set! out o)))

    (comm-in
     (lambda ()
       (d: "uart-in ~x\n"
           ;; (read-byte-timeout i 3)
           (read-byte in)
           )))
    (comm-out
     (lambda (b)
       (write-byte (d: "uart-out ~x\n" b) out)))
    (comm-close
     (lambda ()
       (close-output-port out)
       (close-input-port  in)
       ))
    (comm-reconnect
     (lambda ()
       ;;(printf "close ~s ~s\n" name baud)
       ((comm-close))
       ;;(printf "connect ~s ~s\n" name baud)
       (connect)))

    (connect)
    
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



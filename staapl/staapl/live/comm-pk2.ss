#lang scheme/base
(require
 "../pk2/icsp.ss"
 "../pk2/pk2.ss"
 "../pk2/comm.ss"
 "../pk2/cmd.ss"
 "tethered.ss"
 scheme/system)

(provide
 (all-from-out "../pk2/comm.ss"))

(provide comm-pickit2)

(define (comm-pickit2 dev baud)
  (define (connect)
    (printf "Connecting to PICkit2.\n")
    (pk2-boot)
    (target-on)  ;; Needs to be made safe
    (when baud
      (let ((period (truncate (/ 1000000 baud))))
        (printf "period:   ~a ms (~a baud)\n"
                period baud)
        (icsp-us period))))

  (define (reconnect)
    (pk2-close)
    (sleep 1)
    (connect)
    )
  
  (comm-reconnect reconnect)      ;; PK2 seems buggy..
  (comm-close     pk2-close)      ;; closer programmer connection
  (comm-stat      pk2-stat)       ;; check pk2 status
  
  (comm-reset     pk2-reset)      ;; reset target
  (comm-on        pk2-on)         ;; target VDD on
  (comm-off       pk2-off)        ;; target VDD off

  (comm-poll      pk2-poll)       ;; check if target is listening
  (comm-in        pk2-in)         ;; read & write byte (buffered according 
  (comm-out       pk2-out)        ;; .. to packet protocol)
  

  (connect))



#lang scheme/base
(provide (all-defined-out))
(require
 "util.ss"
 "pk2script.ss"
 "driver.ss"
 )
;; Commands in terms of pk2script.ss


(define (target-on)
  (EXECUTE_SCRIPT (VDD_GND_OFF) ;; disconnect VDD from ground
                  (VDD_ON))     ;; connect VDD to V_target
  (msleep 50))                  ;; wait for power to stabilize


(define (target-off)
  (EXECUTE_SCRIPT (VDD_OFF)        ;; disconnect VDD from V_target
                  (VDD_GND_ON)))   ;; connect VDD to ground

(require scheme/match)
(define (log-status bits . strs)
  (if (null? strs)
      '()
      (cons (list (band 1 bits) (car strs))
            (apply log-status (>>> bits 1) (cdr strs)))))

(define (status)
  (match (pk2 (READ_STATUS))
         ((list lo hi)
          (append
           (log-status lo
                       "Vdd GND"
                       "Vdd"
                       "Vpp GND"
                       "Vpp"
                       "VddError (Vdd < Vfault)"
                       "VppError (Vpp < Vfault)"
                       "Button Pressed")
           (log-status hi
                       "Reset since READ_STATUS"
                       "UART Mode"
                       "ICD transfer timeout/Bus Error"
                       "Script abort - upload full"
                       "Script abort - download empty"
                       "RUN_SCRIPT on empty script"
                       "Script buffer overflow"
                       "Download buffer overflow")))))



(define (print-voltages)
  (define (rounded x) (round (* 1000. x)))
  (for ((v (voltages)))
       (let ((name (cadr v))
             (value (rounded (car v))))
         (printf "~a ~a mV " name value)))
  (newline))

(define (voltages)
  (match (b->w (pk2 (READ_VOLTAGES)))
         ((list vdd vpp)
          `((,(fixedpoint vdd 5.0)  "Vdd")
            (,(fixedpoint vpp 13.7) "Vpp")
            ))))

(define (set-vpp vpp)
  (let ((v (* vpp 18.61)))
    (SETVPP #x40
            (->int v)
            (->int (* v 0.7)))))

(define (set-vdd vdd)
  (let* ((vf (* .80 vdd))
         ;; integer encodings:
         (vddi (<<< (->int (+ (* vdd 32.0) 10.5)) 6))
         (vfi  (->int (* (/ vf 5.0) 255.0))))
    (SETVDD (band vddi #xFF)
            (band (>>> vddi 8) #xFF)
            vfi)))



(define (reset-hold)
  (EXECUTE_SCRIPT (MCLR_GND_ON)))
(define (reset-release)
  (EXECUTE_SCRIPT (MCLR_GND_OFF)))

(define (download . lst)
  (CLR_DOWNLOAD_BFR)
  (apply DOWNLOAD_DATA lst))


;; run script in memory
(define (run uploaded-script-index times)
  (CLR_UPLOAD_BFR)
  (RUN_SCRIPT uploaded-script-index times))

;; run script in usb buffer
(define (execute live-script [times 1])
  (when live-script
    (CLR_UPLOAD_BFR)
    (for ((_ (in-range 1)))
         (EXECUTE_SCRIPT live-script))))
  


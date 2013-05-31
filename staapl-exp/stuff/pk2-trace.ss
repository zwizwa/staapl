
;; From the trace prodoced by pk2cmd this is a set of distilled
;; primitives.

(define (config-pk2)
  (target-off)
  (SETVDD ...)
  (SETVPP ...)
  (SCRIPT_BUFFER_CHKSM)  ;; check if we got the scripts
  (CLR_SCRIPT_BFR)
  (DOWNLOAD_SCRIPT))

(define (target-off)
  (EXECUTE_SCRIPT (VDD_OFF)        ;; disconnect VDD from V_target
                  (VDD_GND_ON)))   ;; connect VDD to ground

(define (reset-hold)
  (EXECUTE_SCRIPT (MCLR_GND_ON)))
(define (reset-release)
  (EXECUTE_SCRIPT (MCLR_GND_OFF)))

(define (target-on)
  (EXECUTE_SCRIPT (VDD_GND_OFF) ;; disconnect VDD from ground
                  (VDD_ON))     ;; connect VDD to V_target
  (msleep 50))                  ;; wait for power to stabilize

(define (download lst)
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
  

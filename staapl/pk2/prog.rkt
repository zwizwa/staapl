#lang racket/base

;; PK2 device programming
(provide (all-defined-out))
(require
 "util.rkt"
 "pk2const.rkt"     ;; PICkit2 bits
 "pk2script.rkt"    ;; command and scripting languages
 "device-file.rkt"  ;; Reader for Microchip's programming scripts
 "cmd.rkt")


;; INTERFACE

;; Accept a chunk list like 'write-ihex.

(define (write-and-verify write read it)
  (let ((bytes (length it)))
    (printf "write/verify ~a bytes\n" bytes)
    (write it)
    (when read
      (let verify ((n 3))
        (let ((read-it (read bytes)))
          (if (null? read-it)
              (unless (zero? n)
                (printf "WARNING: empty read!\n")
                (target-off)
                (msleep 100)
                (verify (sub1 n)))
              (unless (equal-head? it read-it)
                (printf "VERIFY FAILED!\nwrote:\n")
                (dump-list it)
                (printf "read:\n")
                (dump-list read-it))))))))

;; DEBUG

(define (pk2-program progmem configmem [parttype #f])
  (unless configmem
    (printf "WARNING: no configuration found at address ~x\n" (ConfigAddr)))

  (when parttype (part parttype))
  (printf "Programming ~a\n" (part))


  (prepare-for-programming)  
  (begin
    (printf "    erase: all\n")
    (chip-erase)
    (dump-list (read-program-memory 64)))

  (begin
    (printf "  program: ")
    (write-and-verify write-program-memory
                      read-program-memory
                      progmem))

  (begin
    (when configmem
      (printf "   config: ")
      (write-and-verify write-config-memory
                        read-config-memory
                        configmem))))



;; Programming primitives.

(define (set-download-address a)
  (download (band a #xFF)
            (band (>>> a 8) #xFF)
            (band (>>> a 16) #xFF))
  (EXECUTE_SCRIPT (ProgMemAddrSetScript)))


(define (read-device-id)
  (dynamic-wind
      target-on
      (lambda ()
        (EXECUTE_SCRIPT (ProgEntryScript))
        (EXECUTE_SCRIPT (ReadDevIDScript))
        (car (b->w (UPLOAD_DATA))))
      target-off))


(define (prepare-for-programming)
  ;; (READ_STATUS)
  (target-off)
  (set-vdd (VddMax))
  (set-vpp (Vpp))
  (EXECUTE_SCRIPT (SET_ICSP_SPEED 2))       ;; 1uS  

  )



(define (enter-programming [addr #f])
  ;; (target-off)
  (reset-hold)
  (target-on)
  (execute (ProgEntryScript))
  (print-voltages)
  (when addr
    (set-download-address addr)))


(define (leave-programming)
  (execute (ProgExitScript))
  (target-off)
  (reset-release))

(define (read-program-memory [bytes #f])
  (dynamic-wind
      (lambda ()
        (enter-programming 0))
      (lambda ()
        ;; (printf "RdWords ~a\n" (ProgMemRdWords))
        (let ((total-bytes (* (BytesPerLocation) (ProgramMem)))
              (block-bytes (* (BytesPerLocation) (ProgMemRdWords))))
          (collect-size
           (or bytes total-bytes)
           ;; FIXME: address needs to be set on 32k word boundaries
           (lambda ()
             (execute (ProgMemRdScript))
             (collect-size block-bytes UPLOAD_DATA)))))
      leave-programming))

(define (read-config-memory [size-ignored #f])
  (dynamic-wind
      enter-programming
      (lambda ()
        (execute (ConfigRdPrepScript))
        (execute (ConfigRdScript))
        (UPLOAD_DATA))
      leave-programming))



;; FIXME: save badgap and oscal if necessary!
(define (chip-erase)
  (unless (zero? (BandGapMask)) (error 'cant-save-bandgap))
  (unless (zero? (OSCCALSave))  (error 'cant-save-osccal))
  (dynamic-wind
      (lambda ()
        (enter-programming #f))
      (lambda ()
        (execute (ChipErasePrepScript))
        (execute (ChipEraseScript))
        )
      leave-programming))
  

;; orig : (0 200 15 0 0 128 128 0 3 192 3 160 3 64)
;; FIXME: need to disable code protection before writing (CONFIG5). see pk2-3.00/pk2ctrl.c
(define (write-config-memory byte-list)
  (dynamic-wind
      (lambda ()
        (enter-programming))
      (lambda ()
        (execute (ConfigWrPrepScript))
        (apply download byte-list)
        (execute (ConfigWrScript)))
      leave-programming))


(define UPLOAD_BUFFER_SIZE 128)
(define DOWNLOAD_BUFFER_SIZE 256)


(define (write-program-memory byte-list [addr 0])
  (define chunk-size 32) ;; simplest, so it fits in one USB command for DOWNLOAD_DATA
  (define bytes-per-run (* (ProgMemWrWords) (BytesPerLocation)))
  (define script-iterations (/ chunk-size bytes-per-run))

  (unless (zero? (band (sub1 bytes-per-run) addr))
    (error 'bad-memory-alignment "~a" addr))

  (dynamic-wind
      (lambda ()
        (CLR_SCRIPT_BFR)
        (DOWNLOAD_SCRIPT 0 (ProgMemWrScript))
        (enter-programming addr)
        )
      (lambda ()
        (execute (ProgMemWrPrepScript))
        (distribute-size
         chunk-size
         (lambda (short-lst)
           (let* ((lst (pad short-lst chunk-size)))
             ;; (printf "downloading: ~s\n" lst)
             ;; (printf "iterations: ~s\n" script-iterations)
             (apply download lst)
             (run 0 script-iterations)
             ))
         byte-list))
      leave-programming))


;; Reading program memory. Inspired by Jeff Post's pk2. See dev log.

(define (print-read-params)
  (printf "Memory: ~x\nBytes/word: ~s\nBlocksize: ~s\nBlankvalue: ~x\n"
          (ProgramMem)
          (BytesPerLocation)
          (ProgMemRdWords)
          (BlankValue)))


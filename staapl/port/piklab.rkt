#lang scheme/base

(require (lib "process.ss")) ;; system

;; Interface to piklab-prog
(provide (all-defined-out))

(define piklab-firmware     (make-parameter "/home/tom/firmware/icd2"))
(define piklab-device       (make-parameter "18F1220"))
(define piklab-port         (make-parameter "usb"))
(define piklab-self-powered (make-parameter "false"))
(define piklab-misc         (make-parameter "-p icd2 --quiet")) ;; --debug

(define (piklab-prog-cmd command-string)
  (let ((cmd 
         (format "piklab-prog ~a -t ~a --target-self-powered ~a --firmware-dir ~a -d ~a ~a"
                 (piklab-misc)
                 (piklab-port)
                 (piklab-self-powered)
                 (piklab-firmware)
                 (piklab-device)
                 command-string)))
    (display cmd) (newline)
    (void
     (or (system cmd)
         (error 'piklab-prog-error)))))

(define (piklab-prog-program filename)
  (piklab-prog-cmd (format "-c program ~a" filename)))
(define (piklab-prog-run)
  (piklab-prog-cmd "-c run"))

(define (piklab-prog filename)
  (piklab-prog-program filename)
  (piklab-prog-run))
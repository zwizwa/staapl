#lang scheme/base

;; FIXME: replace this with scheme logger facilities

(provide current-log-port
         loading
         log:)

(define current-log-port (make-parameter
                          (current-output-port)
                          ;; #f
                          ))
(define *log* '())

(define (log:
         #:tag [tag #f]
         fmt . args)

  (let ((str
         (apply format
                (string-append
                 (if tag
                     (format "~a: " tag)
                     ";; ")
                 fmt)
                args)))
    (if (current-log-port)
        (display str (current-log-port))
        (set! *log* (cons str *log*)))))
  

;; Code instantiation
(define (loading str)
  (log: "\\__ ~a\n" str))

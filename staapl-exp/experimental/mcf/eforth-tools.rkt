#lang racket/base

(provide (all-defined-out))

;; Trace: print memory access trace. notrace used to disable trace
;; printing in context, i.e. for discarding stack push/pop pointer
;; updates.

(define trace (make-parameter #f))
(define-syntax-rule (notrace . body)
  (parameterize ((trace #f)) . body))
(define (logf . args) (when (trace) (apply printf args)))



#lang racket/base

(require "eforth-tools.ss")
(provide mem! mem@ word word-size)


;; Word size: always using finite bit vectors for memory values.
(define word-size 16)
(define (word x) (bitwise-and #xFFFF x))

;; To get it working, start with word addressed memory.  The idea is
;; that this VM should be only extensible through its memory model
;; (i.e. to add memory mapped channels).

(define mem-size #x10000)
(define mem (make-vector mem-size 0))


(define (mem@ addr)
  (let ((val (word (vector-ref  mem addr))))
    (logf "[~a] -> ~a\n" addr val)
    val))
  
(define (mem! addr . val)
  (for ((a (in-naturals (word addr)))
        (v val))
       (let ((_v (word v)))
         (vector-set! mem a _v)
         (logf "[~a] <- ~a\n" a _v))))



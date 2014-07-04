#lang racket/base

;; These are basic utility functions to be used in base.ss, but not to
;; be re-exported: base.ss should only export base.* names.

(provide (all-defined-out))

(require "rep.rkt"
         "../ns.rkt"
         "stack.rkt"
         )

;; Convenience macro for primitive definitions.
(define-syntax define-word
  (syntax-rules ()
    ((_ name proto . body)
     (ns (scat) (define name 
                  (make-word
                   (stack-lambda proto . body)))))))


;; Like scheme's lambda, but with specified annotation. This is
;; useful for temporary words that have no source representation.
  
;; (define-syntax pn-lambda-annotate
;;   (syntax-rules ()
;;     ((_ annotation formals body ...)
;;      (make-word 
;;       annotation
;;       (lambda formals body ...)))))
        








;; Direct stack access. Use these functions to give proper error
;; handling.

(define (need-pair s)
  (unless (pair? s) (error 'stack-underflow)))

(define (stack-car s) (need-pair s) (car s))
(define (stack-cdr s) (need-pair s) (car s))





;; General state accumulation with proper tail recursion. Maybe this
;; should be called fold? Not really, since it can't express right
;; fold..

;; - abstract interpretation of an abstract list
;; - the last element is called in tail position

;; it's easier to test zero case up front

;; NOTE: symbols are used more than once, so best to use variables.


(define (interpret-list
         interpret         ;; abstract code interpretation
         car cdr null?     ;; abstract list access
         lst               ;; code sequence
         state)            ;; state accumulator
  (if (null? lst)
      state                            ;; nop
      (let next ((l lst)
                 (s state))
        (let ((kar (car l))
              (kdr (cdr l)))
          (if (null? kdr)
              (interpret kar s)        ;; tail call
              (next kdr                ;; recursive call
                    (interpret kar s)))))))


(define (->string x) (format "~a" x))


(define cpm-mark
  (let ((ms 0))
    (lambda ()
      (let*
          ((now   (current-process-milliseconds))
           (delta (- now ms)))
        (set! ms now)
        delta))))

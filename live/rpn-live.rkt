#lang racket/base

;; A late-bound dialect of the scat: language.  Identifiers are
;; translated using the (live ...) mapper macro into code that
;; performs lookup and interpretation at runtime.

;; This language is used to implement the target: language which has
;; similar late-bound semantics.

(require
 "../tools.rkt"
 "../scat.rkt"
 "../rpn.rkt"
 "../ns.rkt"
 (for-syntax
  "../ns-tx.rkt"
  racket/base))

(provide (all-defined-out))



(define (live-interpret sym)
  (define (dbg dict) '(printf "live-interpret: ~a ~a\n" sym dict))
  (define defined? (make-ns-defined? sym))
  (cond
   ((defined? '(scat)) => (lambda (x) (dbg 'scat)   x))
   ((defined? '())     => (lambda (x) (dbg 'scheme) (scat-wrap-dynamic x)))
   (else (error 'undefined "~s" sym))))
   
;; Same as scat: but all identifiers are late-bound.  The main purpose
;; of this macro is to support the target: language which also employs
;; late binding.

(define-syntax-rule (live: code ...)
  (make-word
   (rpn-parse (rpn:-compile    ;; dictionary compiler
               (live)          ;; namespace lookup
               scat-apply      ;; function 
               scat-push       ;; immediate
               scat-push       ;; immediate program
               live:           ;; anonymous compiler for recursive parse
               (rpn-lambda)    ;; dictionary init (only one anonymous entry)            
               ) code ...)))


(define-syntax (live stx)
  (syntax-case stx ()
    ((_ id)
     (identifier? #'id)
     #'(live-interpret 'id))))


;; cfr. scat>
;(define-syntax live>
;  (syntax-rules ()
;    ((_ . code)
;     (stack-print
;      (stack-list
;       ((live: . code) (state:stack)))))))


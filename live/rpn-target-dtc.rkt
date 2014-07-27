#lang racket/base

;; 16bit DTC variant of rpn-target.rkt

(require
 
 "../target.rkt"
 "../scat.rkt"
 "../ns.rkt"
 "../rpn.rkt"
 "../macro.rkt"
 "tethered.rkt"
 "commands.rkt"
 "../forth/forth-lex.rkt"
 "rpn-live.rkt"
 "reflection.rkt"
 (for-syntax
  "../ns-tx.rkt"
  racket/base))

(provide (all-defined-out))

(define-syntax-rule (target-dtc-push  im p sub)
  (let ((p ((live: 'im _>t) p))) sub))

;; FIXME: use different convention for primitives, which are callable
;; from native code, and DTC words which need to run inside DTC
;; interpreter.
(define (underscore sym)
  (string->symbol (format "_~a" sym)))

(define (target-dtc-interpret sym)
  (define (dbg dict x) '(printf "target-dtc-interpret: ~a ~a ~a\n" sym dict x))
  (define _sym (underscore sym))
  (define defined? (make-ns-defined? sym))
  (cond
   ((defined? '(host))      => (lambda (x) x))
   ((target-find-code _sym) => (lambda (x) (dbg 'code x)  (live: ',x _>t ',(target-find-code 'execute/dtc) texec/b)))
   ((target-find-data sym)  => (lambda (x) (dbg 'data x)  (live: ',x _>t)))
   (else                                   (dbg 'live #f) (live-interpret sym))))

(define-syntax-rule (target id)
  (target-dtc-interpret 'id))

(define-syntax-rule (target-dtc: code ...)
  (make-word
   (rpn-parse (rpn:-compile
               (target)
               scat-apply
               target-dtc-push
               scat-push    ;; Program quotations are not used in target language,
               scat:        ;; so they escape to scat.
               (rpn-lambda)
               ) code ...)))

(define-syntax-rule (target-dtc> code ...)
  (begin
    '(printf "(target-dtc> . ~s)\n" '(code ...))
    (void ((target-dtc: code ...) (state:stack)))))

(define-syntax-rule (dtc-forth-command str)
  (forth-lex-string/cps target-dtc> str))




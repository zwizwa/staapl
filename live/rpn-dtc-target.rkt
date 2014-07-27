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

(define-syntax-rule (dtc-target-push  im p sub)
  (let ((p ((live: 'im _>t) p))) sub))

;; FIXME: use different convention for primitives, which are callable
;; from native code, and DTC words which need to run inside DTC
;; interpreter.
(define (underscore sym)
  (string->symbol (format "_~a" sym)))

(define (dtc-target-interpret sym)
  (define (dbg dict x) (printf "dtc-target-interpret: ~a ~a ~a\n" sym dict x))
  (define _sym (underscore sym))
  (define defined? (make-ns-defined? sym))
  (cond
   ((defined? '(host))      => (lambda (x) x))
   ((target-find-code _sym) => (lambda (x) (dbg 'code x)  (live: ',x _>t ',(target-find-code 'execute/dtc) texec/b)))
   ((target-find-data sym)  => (lambda (x) (dbg 'data x)  (live: ',x _>t)))
   (else                                   (dbg 'live #f) (live-interpret sym))))

; 426 = 1AA

(define-syntax-rule (target id)
  (dtc-target-interpret 'id))

;; Abstracted out: used in other target-like language parsers.
(define-syntax-rule (dtc-target-parse (ns push) code ...)
  (make-word
   (rpn-parse (rpn:-compile
               ns
               scat-apply
               push
               scat-push    ;; Program quotations are not used in target language,
               scat:        ;; so they escape to scat.
               (rpn-lambda)
               ) code ...)))
  
(define-syntax-rule (dtc-target: code ...)
  (dtc-target-parse ((target)
                     dtc-target-push)
                    code ...))

;; State is not persistent.
(define-syntax-rule (dtc-target> code ...)
  (begin
    '(printf "(dtc-target> . ~s)\n" '(code ...))
    (void ((dtc-target: code ...) (state:stack)))))

(define-syntax-rule (dtc-forth-command str)
  (forth-lex-string/cps dtc-target> str))




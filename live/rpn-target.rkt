#lang racket/base

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


;; The target: language provides a simulation of a Forth console that
;; is running on the target machine.  The target: form produces a scat
;; function.

;; LITERALS are moved to the target parameter stack.
(define-syntax-rule (target-push  im p sub)
  (let ((p ((live: 'im >t) p))) sub))


;; IDENTIFIERS refer to one of
;;
;;   - prefix parsing macros defined in the (target) dictionary,
;;   escaping the default semantics of `target-interpret' (see
;;   commands.rkt)
;;
;;   - commands defined in the (host) dictionary
;;
;;   - names of on-target binary code in (target) dictionary.  these
;;   will be executed.
;;
;;   - names of composite macros in the (macro) dictionary.  these
;;   will be expanded and attempted to be interpreted as code/data.
;;
;;   - scat functions in (scat) dictionary
;;
;;   - automatically lifted-to-scat scheme functions.


;; Classical Forth doesn't interpret macros at the console, but since
;; Coma Forth is based heavily on macros, interaction would be quite
;; painful without first instantiating some macros as binary code on
;; the target.  Note that this is basicly a hack, and not all macros
;; can be simulated.

;; If identifiers do not have bound syntax, interpretation is delayed
;; until runtime.  The target: language always runs interactively with
;; an associated toplevel namespace for maximum debugging flexibility.


;; FIXME: this prefers code over data, instead of the first one found.
;; Might need to do it differently?

(define (target-interpret sym)
  (define (dbg dict) '(printf "target-interpret: ~a ~a\n" sym dict))
  (define defined? (make-ns-defined? sym))

  (cond
   ((defined? '(host))     => (lambda (x) x))
   ((target-find-code sym) => (lambda (x) (dbg 'code)  (live: ',x texec/b)))
   ((target-find-data sym) => (lambda (x) (dbg 'data)  (live: ',x >t)))
   ;; ((defined? '(macro)) => (lambda (x) (dbg 'macro) (live: ',x tsim)))
   ((defined? '(macro))    => (lambda (x) (dbg 'macro) (target-compile-macro sym) (target-interpret sym)))
   (else                                  (dbg 'live)  (live-interpret sym))))





(define-syntax-rule (target id)
  (target-interpret 'id))

;; Abstracted out: used in other target-like language parsers.
(define-syntax-rule (target-parse (ns push) code ...)
  (make-word
   (rpn-parse (rpn:-compile
               ns
               scat-apply
               push
               scat-push    ;; Program quotations are not used in target language,
               scat:        ;; so they escape to scat.
               (rpn-lambda)
               ) code ...)))
  
(define-syntax-rule (target: code ...)
  (target-parse ((target)
                 target-push)
                code ...))

;; State is not persistent.
(define-syntax-rule (target> code ...)
  (begin
    '(printf "(target> . ~s)\n" '(code ...))
    (void ((target: code ...) (state:stack)))))

(define-syntax-rule (forth-command str)
  (forth-lex-string/cps target> str))




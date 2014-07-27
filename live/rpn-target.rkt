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

;; Native code + DTC on top of native.
(define (native-push data) (live: ',data >t))
(define (native-exec code) (live: ',code texec/b))

(define (dtc-push data) (live: ',data _>t))
(define (dtc-exec code) (live: ',code _>t ',(target-find-code 'execute/dtc) texec/b))
(define (dtc-find sym) 
  (or
   ;; HACK to access primitives which are all defined with
   ;; underscores.  To fix this, find a good way to present a
   ;; collection of names to the live interaction while hiding core
   ;; 8bit words.
   (target-find-code (string->symbol (format "_~a" sym))) 
   (target-find-code sym)))

(define live-push (make-parameter native-push))
(define live-exec (make-parameter native-exec))
(define live-find (make-parameter target-find-code))


;; The target: language provides a simulation of a Forth console that
;; is running on the target machine.  The target: form produces a scat
;; function.

;; LITERALS are moved to the target parameter stack.
(define-syntax-rule (target-push  im p sub)
  (let ((p (((live-push) im) p))) sub))


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
   (((live-find) sym)      => (lambda (x) (dbg 'code)  ((live-exec) x)))
   ((target-find-data sym) => (lambda (x) (dbg 'data)  ((live-push) x)))
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




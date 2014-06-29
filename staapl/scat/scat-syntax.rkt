#lang scheme/base

;; This combines syntax transformers from rpn-*.ss with namespace
;; tools from ns-*.ss into a single interface, together with scheme
;; snarfing and composition macros.

(provide
 compositions ;; scat composer to namespace, using anonymous compiler
 with-compositions

 ;; from snarf.ss
 snarf        ;; scheme snarfer to namespace
 as-void      ;; snarfers
 as-push)

(require
 "../tools.ss"
 "../ns.ss"
 "snarf.ss" 
 (for-syntax
  "../ns-tx.ss"
;;  "../tools.ss"
  scheme/base))


;; The 'compositions' macro is the entry point to the expansion and
;; registration code. It takes a destination namespace and either an
;; anonymous function compiler as an argument. Redefinitions are
;; allowed, and have the 'super' word bound to refer to the previous
;; definition.
  
;; (define-sr (compositions dst-ns fn: (name . body) ...)
;;  (redefinitions!-ns dst-ns (name (fn: . body)) ...))

(define-syntax-rule (compositions dst-ns fn: (name . body) ...)
  (ns dst-ns
      (define-values (name ...)
        (values (fn: . body) ...))))

;; Similar, but create an environment with redefined words.

(define-syntax-rule (with-compositions dst-ns fn: (name . body) ...)
  (lambda (thunk)
    (parameterize-words-ns!
     dst-ns ((name (fn: . body)) ...)
     (thunk))))

;; Scheme snarfing into target namespace with plugin snarfer: (snarfer
;; <scheme-fn> . <formals>) Example snarfers include as-void and
;; as-push, defined in rpn-snarf.


(define-syntax-rule (snarf tx ns (formals (fn ...)) ...)
  (begin (row tx ns (formals (fn ...))) ...))
       
(define-syntax-rule (row tx ns (formals (fn/tofrom ...)))
  (begin (word tx ns formals fn/tofrom) ...))

(define-syntax word
  (syntax-rules ()
    ((_ tx ns a (to from))
     (pre/to/from tx ns to from . a))
    ((_ tx ns a fn)
     (word tx ns a (fn fn)))))

(define-syntax-rule (pre/to/from snarfer n to from . args)
  (ns n (define to (snarfer from . args))))




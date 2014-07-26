#lang racket/unit
(require  "../coma/macro-forth-sig.rkt"
          "../macro.rkt"
          "../label-sig.rkt"
          "dtc.rkt"
          (only-in "../pic18.rkt" ;; Tie into the full pic18 compiler.
                   label:wrap-word
                   label:append!
                   compile!)
          racket/pretty)
(import)
(export macro-forth^)

;; The idea is to feed the syntax back into the native 8bit pic18
;; compiler extended with a dtc interpreter.

;; Two kinds of wrapping need to be done:
;; - Individual words need to be wrapped like this:  (+) = (' _+ _compile)
;; - Definitions need to be wrapped to prefix `enter'

(define (mf:wrap-word name loc inline)
  (printf "dtc mf:wrap-word ~a\n" name)
  (define-values (label
                  invoke
                  wrapped-inline)  ;; inlining macro
    (label:wrap-word name
                     loc
                     (macro: enter ,inline)))
  (values label
          (macro: ',invoke _compile)
          wrapped-inline))

(define (mf:lit datum)
  (macro: ',datum _literal))

(define (mf:reg inline) (label:append! inline))
(define (mf:compile!)   (compile!))

;; Macros and variables not implemented yet.
(define (NI tag)
  (lambda args
    (pretty-print (cons 'NI (cons tag args)))
    (void)))
(define mf:wrap-macro    (NI 'wrap-macro))
(define mf:wrap-variable (NI 'wrap-variable))

 

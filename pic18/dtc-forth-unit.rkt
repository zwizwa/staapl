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

;; The DTC language is implemented on top of the native PIC18 Forth.

;; It reuses the macro-forth^ infrastructure, which is a collection of
;; Scheme macros tied to a collection of compiler words.  The
;; implementation for the DTC delegates to the native PIC18 compiler
;; with a couple of modifications.
;;
;; - Invokation words are abstracted as (macro: ',invoke _compile).
;;   In dtc-lang.rkt the primitives are wrapped in a similar way.
;;
;; - Each definition needs to be prefixed with the `enter' word.
;;
;; - Literals are implemented differently

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

;; Delegate to native compiler.
(define (mf:reg inline) (label:append! inline))
(define (mf:compile!)   (compile!))

;; Macros and variables not implemented yet.
(define (NI tag)
  (lambda args
    (pretty-print (cons 'NI (cons tag args)))
    (void)))
(define mf:wrap-macro    (NI 'wrap-macro))
(define mf:wrap-variable (NI 'wrap-variable))

 

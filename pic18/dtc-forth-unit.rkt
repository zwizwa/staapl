#lang racket/unit
(require  "../coma/macro-forth-sig.rkt"
          "../macro.rkt"
          "../label-sig.rkt"
          "dtc.rkt"
          (only-in "pic18.rkt" ;; Tie into the full pic18 compiler.
                   macro/<<
                   macro/dw>
                   label:allot
                   label:wrap-word
                   label:wrap-variable
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
  ;;(printf "dtc mf:wrap-word ~a\n" name)
  (define-values (label
                  invoke
                  wrapped-inline)  ;; inlining macro
    (label:wrap-word name
                     loc
                     (if name
                         (macro: enter ,inline)
                         inline) ;; skip words before first ':'
                     ))
  (values label
          (macro: ',invoke _compile)
          wrapped-inline))

(define (mf:lit datum)
  (macro: ',datum _literal))


(define (mf:wrap-variable name loc code)
  (define-values (label
                  invoke
                  wrapped-inline)
    (label:wrap-variable name
                         loc
                         (macro: ,code
                                 ;; Here the size is already compiled
                                 ;; as a literal which for dtc is just
                                 ;; a raw word.  Undo that and pass it
                                 ;; on to native allot.
                                 dw>
                                 ,label:allot)))
  (values label
          (macro: ,invoke _literal)
          wrapped-inline))
                         

;; Delegate to native compiler.
(define (mf:reg inline) (label:append! inline))
(define (mf:compile!)   (compile!))

;; Macros are not yet necessary.  Might not need much wrapping except
;; for getting the ';' word to work properly.
(define (mf:wrap-macro name loc code)
  (error 'mf:wrap-macro "not implemented"))
  

 

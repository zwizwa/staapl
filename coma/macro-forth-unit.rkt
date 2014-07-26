#lang racket/unit
(require
 "op.rkt"
 "../macro.rkt"
 "macro-forth-sig.rkt"
 "../sig.rkt"
 "../label-sig.rkt")

;; Here we just piggyback on top of the label^ signature.  Eventually
;; the forth expander should also expand in terms of the `words',
;; `macros' and `variables' form instead of doing the distorted
;; trickery it does now.

(import label^ compiler^)
(export macro-forth^)

(define-syntax-rule (wrap-functions (mf:x label:x) ...)
  (begin (define (mf:x . args) (apply label:x args)) ...))

(wrap-functions
 (mf:compile!      compile!)
 (mf:reg           label:append!)
 (mf:wrap-macro    label:wrap-macro)
 (mf:wrap-word     label:wrap-word))

;; Hack around trouble with syntax certificats for allot.
(define (mf:wrap-variable n l c)
  (label:wrap-variable n l (macro: ,c ,label:allot)))

(define (mf:lit datum)
  (lit datum))

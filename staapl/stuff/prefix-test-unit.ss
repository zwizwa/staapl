#lang scheme/unit
(require "../sig.ss" "sig.ss")
(import stack^)  ;; for macro/+
(export prefix-test^)

(define (macro/plus s) (macro/+ s))



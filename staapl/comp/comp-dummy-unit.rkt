#lang scheme/unit
(require "../sig.ss")

;; For testing Forth

(import)
(export instantiate^)

(define compile-words #f)
(define wrap-word #f)
(define wrap-macro #f)
(define wrap-variable #f)

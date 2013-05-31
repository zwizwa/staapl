#lang scheme/base
(require
 "../rpn.ss")

(provide (all-defined-out))

;; PIC18 specific parsing words. Note that all symbols referred here
;; need to be accessible from this file.

(prefix-parsers
  (macro)

  ;; Compile quotable syntax object (e.g. symbol) to binary Flash blob.
  ;; See string.f
  ;; ((sym: word)   (f-> ` word |bin,|))

  )
#lang racket/base
;; FIXME: this isn't worth much without a populated namespace.

;; For live interaction: just expose the token compiler so it can live
;; next to the native pic18 `forth-begin'.
(require
 (rename-in
  (only-in "dtc-lang.rkt" forth-begin)
  (forth-begin dtc-forth-begin)))
(provide dtc-forth-begin)

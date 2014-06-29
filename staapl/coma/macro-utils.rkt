#lang racket/base

(provide
 (all-defined-out))
(require
 "../scat.rkt")

;; SCAT uitilities for forth macro compilation.

(compositions
 (scat) scat:

 (>tag    swap '() cons cons)
 (comma))



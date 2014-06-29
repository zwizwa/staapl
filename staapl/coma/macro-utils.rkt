#lang scheme/base

(provide
 (all-defined-out))
(require
 "../scat.ss")

;; SCAT uitilities for forth macro compilation.

(compositions
 (scat) scat:

 (>tag    swap '() cons cons)
 (comma))



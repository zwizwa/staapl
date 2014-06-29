#lang racket/base
(require racket/promise)

;; Galois fields and drum patterns..  The crazy idea is that there
;; might be some interesting way to turn the structure of Galois
;; fields into drum patterns.  It doesn't seem like anything but
;; arbitrary assignment, but let's give it a try..

;; GF(p^n) with p prime
;;
;;   p = number of distinct drums
;;
;;   n = number of drums that can sound at the same time.  sounding
;;       the same drum multiple times just means hitting it harder.
;;
;; Picking a p^n automatically fixes the structure of the
;; multiplicative group.  The factorization of p^n-1 will determine
;; the number of possible cycles




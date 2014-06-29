;; Interface to the scat language. This contains main language
;; components as well as transformers.

#lang racket/base

(require
 "rpn.rkt"
 "tools.rkt")

(require/provide
 "scat/rep.rkt"
 "scat/scat-syntax.rkt"
 "scat/scat-base.rkt"
 "scat/scat-control.rkt"
 "scat/stack.rkt"
 "scat/print.rkt"
 "scat/rpn-scat.rkt"
 )


(provide
 (all-defined-out))

;; Definition + evaluation shortcut.
(define-syntax scat>
  (syntax-rules ()
    ((_ . code)
     (stack-print
      (stack-list
       ((scat: . code) (state:stack)))))))



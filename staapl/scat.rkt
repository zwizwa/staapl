;; Interface to the scat language. This contains main language
;; components as well as transformers.

#lang scheme/base

(require
 "rpn.ss"
 "tools.ss")

(require/provide
 "scat/rep.ss"
 "scat/scat-syntax.ss"
 "scat/scat-base.ss"
 "scat/scat-control.ss"
 "scat/stack.ss"
 "scat/print.ss"
 "scat/rpn-scat.ss"
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



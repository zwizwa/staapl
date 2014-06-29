#lang racket/base
(require "rep.rkt"
         racket/pretty)
  
(provide
 stack-print
 print-word
 ;; pretty-exn
 )

;; FIXME: this is an inconsistent mess
(define (unparse tree)
  (let down ((thing tree))
    (cond
     ;; ((word? thing) (word-source thing))
     ((list? thing) (map down thing))
     (else thing))))

(define (stack-print s [fmt " ~s"])
  (printf "<~s>" (length s))
  (for-each
   (lambda (item)
     (printf fmt ;; " ~s"
             (unparse item)))
   (reverse s))
  (printf "\n"))

;; (define (pretty-exn ex)
;;   (cond
;;    ((exn:fail:contract:arity? ex) (exn-message ex))
;;    ((exn:fail:contract? ex)       (exn-message ex))
;;    ((exn:break? ex) '(user-break))
;;    ((exn? ex) (list ex (exn-message ex)))
;;    ((not (list? ex)) (list ex))
;;    (else ex)))

(define (print-word w)
  (pretty-print w)) ;;(word-source w)))


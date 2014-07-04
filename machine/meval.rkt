#lang racket/base
(require (for-syntax racket/base))



;; (define-syntax (foo_ stx)
;;   (define-syntax-rule (with-syntax-staged ((n v) ...) expr)
;;     ;; lift stage so `v' can be a pattern variable whos evaluation is
;;     ;; triggered and placed in the n pattern variable.
;;     #`(let-syntax  
;;           ((m (lambda (stx)
;;                 (with-syntax ((n v) ...)
;;                   expr))))
;;         ;; lower stage
;;         (m))) 
;;   (syntax-case stx ()
;;     ((_  e)
;;      (with-syntax-staged
;;       ((v e))
;;       (if (zero? v) 'zero 'e)))))


(define-syntax (foo stx)
  (define-syntax-rule (let-staged ((n v) ...) body ...)
    #`(let-syntax
          ((m (lambda (stx)
                (let ((n v) ...) body ...))))
        (m)))
  (syntax-case stx ()
    ((_ e)
     (let-staged ((v e))
                 (if (zero? v)
                     #'zero
                     #`e)))))


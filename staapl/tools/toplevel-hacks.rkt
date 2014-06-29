#lang racket/base
(provide (all-defined-out))

(require (for-syntax racket/base))

;; Workaround to fish something from the transformer environment in an
;; interactive session.  There has to be another way..

(define-syntax (slv stx)
  (syntax-case stx ()
    ((_ id)
     (datum->syntax
      #f
      #`'#,(syntax-local-value #'id)))))

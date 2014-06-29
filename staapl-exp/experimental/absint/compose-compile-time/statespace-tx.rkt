#lang scheme/base

(provide (all-defined-out))

(define-struct model-meta (state in out))
(define (model-prefix stx)
  (datum->syntax
   stx (string->symbol
        (format "model:~s"
                (syntax->datum stx)))))

(define (model-info id)
  (syntax-local-value (model-prefix id)))
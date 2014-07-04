#lang racket/base
(provide (all-defined-out))
(require
 (for-template "../rpn.rkt")
 "../tools/stx.rkt"
 "../forth/lexer-tx.rkt"
 "../rpn.rkt"
 "../forth/forth-tx.rkt")


(define (stx->path it)
  (let ((it (syntax->datum it)))
    (cond
     ((symbol? it) (string->path (symbol->string it)))
     ((string? it) (string->path it))
     ((path? it)   it)
     (else
      (raise-syntax-error #f "can't convert to path" it)))))

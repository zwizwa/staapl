#lang scheme/base
(provide (all-defined-out))
(require
 (for-template "../rpn.ss")
 "../tools/stx.ss"
 "../forth/lexer-tx.ss"
 "../rpn.ss"
 "../forth/forth-tx.ss")


(define (stx->path it)
  (let ((it (syntax->datum it)))
    (cond
     ((symbol? it) (string->path (symbol->string it)))
     ((string? it) (string->path it))
     ((path? it)   it)
     (else
      (raise-syntax-error #f "can't convert to path" it)))))

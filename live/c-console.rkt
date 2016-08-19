#lang racket/base
(require staapl/ns
         staapl/target/rep)

(provide (all-defined-out))

;; Convert target words into assoc list of (name . addr)
(define-syntax-rule (words->dict word ...)
  (for/list ((n '(word ...))
             (w (list (ns (target) word) ...)))
    `(,n . ,(target-word-address w))))

(define-syntax-rule (struct-init word ...)
  (dict->struct-init (words->dict word ...)))

;; Generate C structure initializer from assoc list
(define (dict->struct-init addrs)
  (printf "#define WORDS {")
  (for ((item addrs))
    (printf "{.name=~s,.addr=~s}," (symbol->string (car item)) (cdr item)))
  (printf "}\n"))

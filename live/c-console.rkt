#lang racket/base
(require staapl/ns
         staapl/target/rep)

(provide (all-defined-out))

;; Convert target words into assoc list of (name . addr)
(define-syntax-rule (words->dict word ...)
  (for/list ((n '(word ...))
             (w (list (ns (target) word) ...)))
    `(,n . ,(target-word-address w))))

;; Generate C structure initializer from assoc list
(define (print-dictionary-struct-init addrs)
  (printf "#define DICTIONARY {")
  (for ((item addrs))
    (printf "{.name=~s,.addr=~s}," (symbol->string (car item)) (cdr item)))
  (printf "{}}\n"))


;; See app/4-relay-module.gen-h.rkt

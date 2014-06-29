#lang racket/unit
(require "sig.rkt"
         "label-sig.rkt"
         "ns.rkt"
         "code.rkt")

;; Translate a bunch of interfaces into one that is used to implement
;; the macros.

(import jump^           ;; exit
        ram^            ;; allot
        org^            ;; org-begin, org-end
        instantiate^)   ;; wrap-word, wrap-variable

(export label^)


;; NOTE: you can't assign the value of an identifier to another one in
;; a module body, as it will be #<undefined>.  I.e. this doesn't
;; always work:
;;    (define label:register! code-append-postponed!)

;; I did manage to do this for other variables, so maybe it depends on
;; the order of the units when the variables are finally bound?  In
;; any case, wrapping function variables in abstraction


(define-syntax-rule (wrap-functions (mf:x label:x) ...)
  (begin (define (mf:x . args) (apply label:x args)) ...))

(wrap-functions
 (label:append!        code-append-postponed!)
 (label:wrap-word      wrap-word)
 (label:wrap-variable  wrap-variable)
 (label:wrap-macro     wrap-macro))


;; The signature macros will insert these words in the RPN code.  They
;; implement access to the target code graph, which is essentially
;; built from a stream of RPN words.
(define-syntax-rule (wrap-macros (l:n n) ...)
  (begin (define (l:n s) ((ns (macro) n) s)) ...))
(wrap-macros
 (label:exit      exit)
 (label:allot     allot)
 (label:org-begin org-begin)
 (label:org-end   org-end))

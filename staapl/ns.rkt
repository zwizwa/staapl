#lang racket/base

;; Abstract the global name space mechanism.

(require
;; "tools.rkt"
 "ns-tx.rkt"      ;; for reflection
 racket/shared
 racket/provide-syntax
 racket/require-syntax
 (for-template
  racket/base)
 (for-syntax
  racket/base
  syntax/stx
  "ns-tx.rkt"
  ))

(provide
 (all-defined-out))

;; Require / provide.

;; Implemented by the catch-all transformer.  These are called #'ns-out
;; and #'ns-in because #'ns is already taken.
(define-provide-syntax ns-out ns-tx)
(define-require-syntax ns-in  ns-tx)
        


;; Binding forms and references.

;;  (ns namespace identifier)   -> transforms variable reference
;;  (ns namespace binding-form) -> transforms variable binding form

(define-syntax ns ns-tx)




;; Reflection

;; Run time access uses symbols.
(define (ns-name ns [name '||])
  (syntax->datum
   (ns-prefixed (datum->syntax #f ns)
                (datum->syntax #f name))))

;; Unwrap the name, #f if not in ns.
(define (ns-name? ns)
  (let* ((prefix (symbol->string (ns-name ns)))
         (lp (string-length prefix)))
    (lambda (sym)
      (let* ((str (symbol->string sym))
             (l (string-length str)))
        (and (> l lp)
             (string=? prefix (substring str 0 lp))
             (string->symbol (substring str lp)))))))
  

;; Find all prefixed words in current namespace.
(define (ns-mapped-symbols ns)
  (let ((basename (ns-name? ns)))
    (foldl
     (lambda (sym collect)
       (let ((it (basename sym)))
         (if it (cons it collect) collect)))
     '()
     (namespace-mapped-symbols))))


(define (make-ns-defined? sym)
  (lambda (ns)
    (namespace-variable-value
     (ns-name ns sym) #t (lambda () #f))))

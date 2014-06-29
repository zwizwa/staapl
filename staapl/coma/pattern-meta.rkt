#lang racket/base

(require
 "pattern.rkt"
 (for-template
  racket/base)
 (for-syntax
  racket/base
  ;; "../tools.rkt"
  ))
(provide
 patterns-class
 meta-pattern)

;; META PATTERNS

;; To create classes of pattern transformers. The code
;;
;;   (meta-pattern unary (word opcode)
;;     (([movf f 0 0] word) ([opcode f 0 0]))
;;     ((word)              ([opcode 'WREG 0 0])))
;;
;;   (unary (macro)
;;          (1+     infc))
;;          (1-     decf))
;;
;; expands to:
;;
;;   (begin
;;     (patterns (macro)
;;       (((movf f 0 0) 1-) ((decf f 0 0)))
;;       ((1-)              ((decf 'WREG 0 0))))
;;     (patterns (macro)
;;       (((movf f 0 0) 1+) ((incf f 0 0)))
;;       ((1+)              ((incf 'WREG 0 0)))))
;;


;; For macros with more than one level of quoting I avoid ellipsis and
;; quasisyntax/unsyntax, and use explicit list ops and nested syntax +
;; syntax-case to bind generated code to pattern variables.

(define-syntax (meta-pattern stx)
  (syntax-case stx ()
    ((_ par-pattern formals . body)
     #'(define-syntax (par-pattern stx)
         ;; Expands to body specializer.
         (define (gen-body args)
           (syntax-case args () (formals #'body)))
         ;; Apply generated specializer params.
         (syntax-case stx ()
           ((_ ns . arg-lists)
            (syntax-case
                (for/list ((a (syntax->list #'arg-lists)))
                 (syntax-case (gen-body a) ()
                   (pb #'(patterns ns . pb))))
                ()
              (pats #'(begin . pats)))))))))


;; In most cases meta patterns don't need to be named.
(define-syntax-rule
  (patterns-class ns formals specs . body)
  (begin
    (meta-pattern P formals . body)
    (P ns . specs)))
  

#lang racket/base


(require
 "../tools.rkt"
 "../ns.rkt"
 (for-syntax
  ;; "../tools.rkt"
  "../tools/stx.rkt"
  "../ns-tx.rkt"
  racket/base
  "static.rkt")
  racket/provide-syntax
  "../ns.rkt")

(provide op-combine-out
         define-op-signature
         op)

(define-syntax-rule (op name ...) (ns (op name ...)))

(define-provide-syntax op-combine-out
  (lambda (stx)
    (syntax-case stx ()
      ((_ name ...)
       #`(combine-out
          (ns-out (op info) (combine-out name ...))    ;; static info
          (ns-out (op ?)    (combine-out name ...))    ;; predicate
          (ns-out (op asm)  (combine-out name ...))    ;; asm instances
          (ns-out (op dasm) (combine-out name ...))    ;; dasm instances
          )))))

;; ASSEMBLY LANGUAGE DECLARATION

;; Defines an op declaration (used by macro transformer definitions
;; like the 'patterns form) and an op implementation.

(define-syntax (define-op-signature stx)
  (syntax-case stx ()
    ((_ sig^ (name arg ...) ...)
     (let ((static-sig-ids
            (for/list ((n (in-stx #'(name ...)))
                       (a (in-stx #'((arg ...) ...))))
              #`(define-syntaxes (#,(ns-prefixed #`(op info) n))
                  (make-op-static '#,a)))))
       #`(begin
           (define-signature sig^
             (#,@static-sig-ids
              #,@(for/list ((n (in-stx #'(name ...))))
                   (map (lambda (ns) (ns-prefixed ns n))
                        (list #'(op ?)     ;; op instance predicate
                              #'(op asm)   ;; associated assembler semantics ..
                              #'(op dasm)  ;; .. and its inverse
                              ))))))))))

;; Note that it might make more sense to use (op ADD info) instead of
;; (op info ADD) following the rationale that 'info is a property of
;; 'ADD.  However, this would complicate the implmementation of the
;; 'ns form which takes lexical context from the last element in a
;; composite name.

  
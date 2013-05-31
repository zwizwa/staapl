;; An RPN syntax for creating peephole optimizing code generator
;; macros using pattern matching applied to the already generated
;; assembly code.

;; The asm buffer can be seen as a stack of typed values, and a
;; 'asm-transforms' specification is a definition of forth words
;; (macros) operating on this stack. For the PIC18 this contains a
;; subset language mapping [qw] -> [qw], acting as a compile time
;; evaluator.

#lang scheme/base


(require
 ;; scheme/match
 "../scat.ss"
 "pattern-runtime.ss"
 (for-syntax
  scheme/base
  "pattern-tx.ss"))

(provide
 asm-pattern
 with-patterns
 patterns
 pattern-lambda)

(define-syntax (patterns stx)
  (syntax-case stx ()
    ((_ namespace . patterns)
     (asm-transforms-tx #'namespace #'patterns))))

(define-syntax (with-patterns stx)
  (syntax-case stx ()
    ((_ namespace . patterns)
     (with-asm-transforms-tx #'namespace #'patterns))))

(define-syntax (pattern-lambda stx)
  (syntax-case stx ()
    ((_ . patterns)
     (asm-lambda-tx #'patterns))))

;; perform RHS quoting for pattern matching.
(define-syntax (asm-pattern stx)
  (syntax-case stx ()
    ((_ . ins)
     (asm-template-tx #'ins #'(printf "asm-pattern\n")))))


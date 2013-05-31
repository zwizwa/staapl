#lang scheme/base

;; Lazy partial evaluation built on the PIC18 forth.
(require
 "rpn.ss"
 "macro.ss"
 "ns.ss")
(require
 "pic18.ss")

;; The idea is to build a lazy layer on top of the strict peephole
;; optimizer, to see what the problems are.  This is inspired by [1].

;; [1] http://lukepalmer.wordpress.com/2009/05/04/lazy-partial-evaluation/

;; Stack-effect annotated macros.
(define-virtual-ops
  (mw a))

;; Literals are wrapped in macros ( == code promises).
(define-syntax-rule (lazy-push val p sub)
  (scat-apply (macro: ',val delay) p sub))

(define-syntax-rule (lazy expr) (ns (lazy) expr))

(define-syntax-rule (lazy: code ...)
  (make-word
   (rpn-parse (rpn:-compile    ;; dictionary compiler
               (lazy)          ;; namespace
               scat-apply      ;; function = same as scat:
               lazy-push       ;; immediate
               lazy-push       ;; immediate program ???
               lazy:           ;; anonymous compiler for recursive parse
               (rpn-lambda)    ;; dictionary init (only one anonymous entry)
               ) code ...)))

  
;; Access promises from the macro language.
(patterns
 (macro)

 (([qw a] delay)    ([mw (macro: ',a)]))
 (([mw m] force)    m)
 
 )

(patterns
 (lazy)
 (([mw a] [mw b] +) ([mw (macro: ,a ,b +)]))
; (([mw a] +) ([mw (macro: ,a +)]))
 )


;; Test with: (pic18-begin ,(lazy: 123 123 +) force)

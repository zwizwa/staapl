#lang scheme/base

;; Bundles lowlevel macro code and adds some convenience functions.

(require
 "../tools.ss")
(provide
 (all-from-out "../tools.ss"))

(require/provide
 "../ns.ss"
 "../scat.ss"
 "../sig.ss"
 "../op.ss"
 "pattern.ss"
 "pattern-meta.ss"
 "pattern-runtime.ss"
 "macro-utils.ss"
 "rpn-macro.ss"
 "macro-eval.ss"
 "op.ss"       ;; compile + literal
 "../target.ss"
 "target-scat.ss"  ;; target:
;; "../asm/directives.ss"
 )

(provide (all-defined-out))


;; Universal list -> macro convertor: each element is quoted and
;; posprocessed with a glue macro. This can be used to construct
;; tables or simple embedded point-free languages.
(define (list->macro m lst)
  (scat-compose
   (map (lambda (el) (macro: ',el ,m)) lst)))

(define (table->macro m rows)
  (scat-compose
   (map (lambda (row)
          (scat-compose
           (append (map (lambda (el) (macro: ',el)) row)
                   (list m))))
        rows)))

;; Convert a stack of immediates in list form to a macro that can
;; recreate the stack.
(define (stack->macro s)
  (apply compose (map (lambda (x) (macro: ',x)) s)))

;; Note that the 2 functions above are not the same!  The first
;; translates some form of sequential code to a macro, while the
;; second translates a state (stack contents) to a macro.


(define macro-word? word?)

(define-syntax-rule (table: separator (item ...))
  (macro: ,(macro: item separator) ...))

;; Same, but given a list intead of syntax.
(define (macro-table separator lst)
  (scat-compose
   (map (lambda (x)
          (macro: ',x ,separator))
        lst)))

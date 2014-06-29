#lang racket/base

(require "../scat.rkt")

;; Static data type for assembler.  The pattern matching and
;; construction look like:

;;   (struct asm/addlw (literal))
;;   (asm/make-addlw literal)

;; There are 2 uses for assembly instructions:

;;   * as data structure supporting pattern matching and construction.
;;   * as reducable expression

;; A pseudo instruction is non-reducable.
;; Assembly requires the association of the data structure to a reducer.

;; Let's make all assembler structs carry a pointer to a reducer, and let
;; them derive from an abstract assembler opcode which has only a
;; reducer.  Make sure the pattern matching form ignores the reducer when
;; matching, and provide a constructor that creates a proper reducer
;; thunk.

;; (define struct:asm
;;   (let-values
;;       (((asm
;;          make-asm
;;          asm?
;;          asm-ref
;;          asm-set!)
;;         (make-struct-type 'asm #f 1 0)))
;;     asm))

(define-struct base-asm (semantics))

(require (for-syntax racket/base))

(define-syntax (define-assembler stx)
  (syntax-case stx ()
    ((_ operator (operand ...) semantics)
     #`(begin
         (define-struct (asm base-asm) (operand ...))
         (define asm-class semantics)
         (ns: (asm type) (define-syntax operator #'asm)) ;; static info for match
         (ns: (asm make) (define operator make))  ;; constructor
         ))))
    
       
#lang scheme/base

;; Run-time objects representing assembler/disassembler.  Also used in
;; pattern matching rules.

(require
 "../tools.ss"
 "op.ss"
 (for-syntax
  ;; "../tools.ss"
  "../ns-tx.ss"
  scheme/base
  "static.ss")
  scheme/provide-syntax
  "../ns.ss")

(provide
 ;; asm/dasm function tags
 (struct-out asm)
 (struct-out dasm)
 ;; asm:

 define-op
 
 define-virtual-ops
 define-lowlevel-op
 define-lowlevel-ops ;; aggregate

 (op-combine-out dw here)

 asm:
 op
 op:
 op-apply
 )


(define-struct asm (fn name))
(define-struct dasm (fn))

(define-syntax-rule (asm: name) (ns (op asm) name))

(define-syntax (op: stx)
  (syntax-case stx ()
    ((_ rator . rands)
     (begin
       (op-check-syntax #'(rator . rands))
       #`(list (asm: rator) . rands)))))
  



(define ((asm-predicate name) x)
  (let* ((n (asm-name x))
         (p (eq? name n)))
    ;; (printf "asm-pred: ~a ~a ~a\n" n (if p "=" "!=") name)
    p))


;; LOWLEVEL PRIMITVE ASSEMBLER DEF

;; Main definer body for asm/dasm/op namespaces.  If the (op)
;; namespace already contains a declaration, it is checked, otherwise
;; it is created.
(define-syntax (define-op stx)
  (syntax-case stx ()
    ((_ name formals asm-body dasm-body)
     (let ((op-name (ns-prefixed #'(op info) #'name)))
     #`(begin
         #,@(let ((local-static (syntax-local-value op-name (lambda () #f))))
              (if local-static
                  (begin ;; FIXME: need to check if it's the same!!
                    ;; (unless ....)
                    '())
                  ;; define it
                  (list #'(ns (op info) (define-syntax name (make-op-static 'formals))))))
         (ns (op ?)    (define name (asm-predicate 'name)))
         (ns (op asm)  (define name (make-asm asm-body 'name)))
         (ns (op dasm) (define name (and
                                     dasm-body
                                     (make-dasm
                                      (dasm-body
                                       (ns (op asm) name))  ;; bind it to assembler instance
                                      )))))))))

(define-syntax-rule (define-lowlevel-op (name addr . formals) . body)
  (define-op
    name
    formals
    (let ((name (lambda (addr . formals) . body))) name)
    #f))


(define-syntax-rule (define-lowlevel-ops (def ...) ...)
  (begin (define-lowlevel-op def ...) ...))


;; Pseudo ops will pass the pattern-tx.ss check, but return run-time
;; errors when used as assembler.  FIXME: this needs to change to an
;; instruction without attached primitive assembler.
(define-syntax-rule (define-virtual-ops (op . args) ...)
  (begin
    (begin
      ;; (printf "virtual: ~a\n" 'op)
      (define-lowlevel-op (op addr . args)
        (error 'asm-pseudo-op "~s" 'op))) ...))




;; Some primitive operations necessary in coma/language.ss
(define-lowlevel-op (here addr)         `(,addr))
(define-lowlevel-op (dw   addr w)       `(,(int w)))


;; Performing assembly.
(define (op-apply ins start-address)
  (apply (asm-fn (car ins)) start-address (cdr ins)))

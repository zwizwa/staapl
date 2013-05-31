#lang scheme/base
(require scheme/unit
         scheme/match
         (for-syntax scheme/base))
(provide
 num^    ;; Number domain signature
 run^    ;; Analysis signature

 ;; Snarf expressions from model+domain unit.
 num-snarf
 num-run 
 num-runs

 ;; Abstract number domain instances
 num-eval@
 num-expr@
 num-anf@
 )

;; The model is "sandwiched" between the basic domain num^ on which it
;; depends, and the analysis stub which implements basic domain and
;; analysis context.  To simplify the code, each model^ exports a
;; signle function `run'.

(begin-for-syntax
 ;; For wrappers used in `define-syntaxes`.  Currently these are just
 ;; used to rename the standard arithmic ops to signature names and to
 ;; check arity.  FIXME: add contracts.  The rename is necesessary to
 ;; be able to use the original bindings to implement compile time
 ;; evaluation.

 ;; FIXME: add contracts + multi-arity 
 (define (((op-arity n) id) stx)
   (syntax-case id ()
     (op-id
      (syntax-case stx ()
        ((op . args)
         (unless (= n (length (syntax->list #'args)))
           (raise-syntax-error #f "arity error" stx))
         #`(op-id . args))))))
 (define (op-wrappers op stx)
   (apply values (map op (syntax->list stx)))))


;; Basic domain.  The ops are defined such that they map relatively
;; well to what is available on a DSP.
(define-signature num^
  (with-num                      ;; setup dynamic evaluation context
   lit                           ;; constant representation
   add sub mul div               ;; arithmetic
   flr                           ;; unary
   (define-syntaxes (+ - * / )   ;; standard Scheme name aliases
     (op-wrappers (op-arity 2) #'(add sub mul div)))
   (define-syntaxes (floor)
     (op-wrappers (op-arity 1) #'(flr)))
   ))

;; Standard analysis interface.
(define-signature run^ (run))
  

;; Tools
(define-syntax-rule (define-ops ops fn)
  (define-values ops (apply values (map fn 'ops))))

;; Basic implementations of the sandwich model

;; Straightforward evaluation.
(define-unit num-eval@
  (import)
  (export num^)
  (define (with-num thunk) (thunk))
  (define (lit x) x)
  (define-values
    (add sub mul div flr)
    (values + - * / floor))
  )

;; Expression compilation
(define ((expr-op sym) . args) (cons sym args))
                           
(define-unit num-expr@
  (import)
  (export num^)
  (define (with-num thunk) (thunk))
  (define (lit x) `(lit ,x))
  (define-ops (add sub mul div flr) expr-op)
  )


;; ANF compilation.  Note that because of the side effects, the result
;; depends on the Scheme evaluation order.

(define anf-dict (make-parameter #f))
(define anf-tag  (make-parameter #f))

;; Note that it is OK to update dynamic _bindings_ as is done below.
;; However it is not a good idea to desctructively modify the _values_
;; these bindings point to, as this messes with the ability to use
;; partial continuations to jump in and out of the context setup by
;; `with-num'.

(define (anf-let! expr)
  (let ((tag (anf-tag)))
    (anf-dict (cons (cons tag expr) (anf-dict)))
    (anf-tag (add1 tag))
    `(var ,tag)))
         
(define (anf-op sym)
  (lambda args
    (let ((expr (cons sym args)))
      (anf-let! expr))))

(define (anf-print dict)
  (for ((line (reverse dict)))
    (printf "~s = ~s\n" (car line) (cdr line))))

(define-unit num-anf@
  (import)
  (export num^)
  (define-ops (add sub mul div flr) anf-op)
  (define (lit x) `(lit ,x))
  (define (with-num thunk)
    ;; Setup evaluation context.
    (parameterize ((anf-dict '())
                   (anf-tag 0))
      ;; Invoke & gather dict.
      (call-with-values thunk
        (lambda retvs
          (values (anf-dict) retvs))))))




;; Tools for code analysis.


;; Execute a function expression in the context of a model bound to a
;; num^ instance.  This uses `with-num' to provide the proper context.

;; This exposes a function interface to be able to abstract the setup
;; of the num^ instance's dynamic context installed by `with-num'.

;; Note that this breaks the outside use of closures created inside
;; this context.  Use partial continuations instead.

;; For info on the `link' form see:
;; http://docs.racket-lang.org/guide/firstclassunits.html


(define-syntax-rule (num-snarf num@ model@ model^ fn-expr)
  (let ((snarf@
         ;; Define a unit used to fish out an expression.  It is kept
         ;; as the last expression on the `link' line below so its
         ;; body will be executed last, making up the return value of
         ;; `invoke-unit'.
         (unit
           (import num^ model^)
           (export)
           (lambda args 
             (with-num
              (lambda ()
                (apply fn-expr args)))))))
    (invoke-unit
     (compound-unit
      (import)
      (export NUM RUN)
      ;;     export-bindings  unit    import-bindings
      ;;     ----------------------------------------
      (link [((NUM : num^))   num@    RUN]
            [((RUN : model^)) model@  NUM]
            [()               snarf@  NUM RUN])))))
  

;; Like `num-snarf' but linked to the run^ signature.
(define-syntax-rule (num-run num@ run@)
  (num-snarf num@ run@ run^ run))

;; Conveniently create multiple interpretations.
(define-syntax-rule (num-runs (@ ...) model@)
  (list (num-run @ model@) ...))



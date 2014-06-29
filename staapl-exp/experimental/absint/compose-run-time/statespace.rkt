#lang racket/base
(require
 racket/stxparam
 racket/pretty
 (for-syntax
  racket/pretty
  racket/base
  "debug-tx.ss"
  "statespace-tx.ss"))

;; State space model.

;; Basic representation of a state space model.  The state, input,
;; output names are used to perform vector pack/unpack and provide a
;; (s i) -> (s o) vector interface for a named node input syntax.

(provide
 model-lambda
 model-update)
(define-syntax-parameter model-update #f)

;; Primitive node representation.  Bundles a set of expressions that
;; specify i/o behaviour, with part of i/o designated as feedback
;; state.

(define nodes vector)
(define nodes-ref vector-ref)

(define-struct model (function nstate nin nout))
                      
(define-syntax (model-lambda stx)
  (define slength (compose length syntax->datum))
  (define (unpack vec ids)
    (for/list ((id (syntax->list ids))
               (n (in-naturals)))
      #`(#,id (nodes-ref #,vec #,n))))
  (syntax-case stx ()
    ((_ (state in out)
        . body)
     #`(syntax-parameterize
        ((model-update
          (lambda (stx)
            (define (context sym)
              (datum->syntax stx sym))
            (syntax-case stx ()
              ((_ bindings)
               #`(let bindings
                     (values
                      (nodes #,@(map context 'state))
                      (nodes #,@(map context 'out)))))))))
        (make-model
         (lambda (vstate vin)
           (let (#,@(unpack #'vstate #'state)
                 #,@(unpack #'vin    #'in))
             . body))
         #,(slength #'state)
         #,(slength #'in)
         #,(slength #'out))))))


;; A composite model
(define-syntax (composite-model stx)
  (syntax-case stx ()
    ((_ in out
        ((node type) ...) ;; model nodes
        ((p pn c cn) ...) ;; producer to consumer links
        )
     #'123)))




;; The following defines compile time information that can be used to
;; patch up state space models.

(begin-for-syntax
 (define models '())
 (define (push-models! k v)
   (set! models (cons (cons k v) models))))


(provide define-model)
(define-syntax (define-model stx)
  (syntax-case stx ()
    ((_ (name state in out) . body)
     (begin
       (push-models! #'name
                     (map syntax->list
                          (list #'state #'in #'out)))
       #`(define name
           (model-lambda (state in out) . body))))))

(define-syntax (debug-models stx) #`'#,models)


;; ******* EXPERIMENTAL ********

;; The following do some more AST juggling.  They distill lambda
;; representations from a minimal specification.

;; FIXME: write on top of `model-lambda'

;; *** (1) function -> state space model transformer using explicit
;;         output names

;; This macro transforms a pure function into a state space
;; formulation which transforms an input stream into an output stream,
;; updating an internal state.

(provide state-space)
(define-syntax (state-space stx)
  (syntax-case stx ()
    ;; Link in terms of node names.
    ((_ ((s i) (s+ o)) ;; node names for state space form
        (f fi fo)      ;; pure function body
        )
     (let* ((-> syntax->list))
       #`(lambda (#,@(-> #'s)
                  #,@(-> #'i))
           (let-values ((fo (f . fi)))
             (values #,@(-> #'s+)
                     #,@(-> #'o))))))))


;; *** (2) AST analysis to find input,output,state based on occurance.

;; State space model: automatic connect & sort for a set of equations.

;; Lambda State Space Equations

(define-syntax (lambda-sse stx)
  (syntax-case stx ()
    ((_ . equations)
     (** eqs-vector-lambda
        (parse-eqs-sets #'equations)))))
                       

;; test
;(define f
;  (lambda-sse
;   (y (+ a b))
;   (z (* y y))))



;; Alternative


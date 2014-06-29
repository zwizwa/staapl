#lang racket/base
(require
 racket/stxparam
 racket/pretty
 (for-syntax
  "statespace-tx.ss"
  racket/pretty
  racket/base))

(provide model-lambda
         model-update)
(define-syntax-parameter model-update #f)


;; In terms of a list of state, input and output node names, create a
;; function that computes (state , input) -> (state , output).

;; The function takes all input nodes concatenedated and produces
;; output nodes as multiple values.  Compile time information below is
;; used to create compositions of models.

(define-syntax (model-lambda stx)
  (define slength (compose length syntax->datum))
  (define (unpack vec ids)
    (for/list ((id (syntax->list ids))
               (n (in-naturals)))
      #`(#,id (nodes-ref #,vec #,n))))
  (syntax-case stx ()
    ((_ ((s ...) (i ...) (o ...)) . body)
     #`(syntax-parameterize
        ((model-update
          (lambda (stx)
            (define (context sym)
              (datum->syntax stx sym))
            (syntax-case stx ()
              ((_ bindings)
               #`(let bindings
                     (values #,@(map context '(s ...))
                             #,@(map context '(o ...)))))))))
        (lambda (s ... i ...)
          . body)))))

 
;; Create a model-lambda bound to a name + transformer bindings about
;; the state/input/output spec.

;; At compile time we keep track of node names.  Combining primitive
;; models with compositions, there needs to be a way to uniquely
;; identify nodes, taking into account recursive composition.
;; Essentially we have 2 ways to deal with this: use composite
;; symbols, i.e. `foo:bar' or `foo.bar', or use lists of symbols.  It
;; seems that composite symbols are easier to work with, as they are
;; mostly for human interpretation; the machine-use just performs
;; concatenation, not decomposition of names.

(define-syntax (define-model stx)
  (syntax-case stx ()
    ((_ name (s i o) . body)
     #`(begin
         (define-syntax #,(model-prefix #'name) '(s i o))
         (define name (model-lambda (s i o) . body))))))



(define-syntax (debug-model stx)
  (syntax-case stx ()
    ((_ id) #`'#,(model-info #'id))))

         

(define-model foo ((s) (i) (o)) 123)
(debug-model foo)


;; Roadmap:
;;  - write a lower-level shared-expression (DAG) version first
;;  - finish the automatic patching macro below

;; (define-syntax (composite-model stx)
;;   (syntax-case stx ()
;;     ((_ name (i o)
;;         ((instance model) ...)  ;; set of child models
;;         ((src sn dst dn) ...)   ;; src -> dst node connections
;;         )

;;      ;; All named models have named state/input/output nodes.  This
;;      ;; also goes for a composite model.  The composite's i/o nodes
;;      ;; are specified in the input form.  The state nodes are
;;      ;; collected from sub-compoments.  These need renaming to make
;;      ;; them unique, as there might be multiple subcomponents of the
;;      ;; same type.  A straightforward solution is to prefix all state
;;      ;; nodes with the instance names used in the input form.  We can
;;      ;; do the same for internal nodes.
     
;;      (let* ((specs (map model-info (syntax->list #'(model ...))))
;;             (tags (syntax->datum #'(instance ...)))
;;             (dict (make-hash (map cons tags specs))))
     

;;      (let ((instances
;;             (syntax->list #'(instance ...))

     
;;      #`(define-model name (#,state i o)


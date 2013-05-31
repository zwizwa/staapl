#lang scheme/base

;; Syntax transformers for defining machines (small-step operational semantics).

;; The idea is to reference registers by name, at compile time it is
;; easily determined which registers are used and which are modified.

;; Syntax will be pattern matching based (field pattern result) which
;; reads easier than positionally encoded struct matches.

(require
 scheme/match
 scheme/control
 scheme/struct-info
 (for-template scheme/base
               scheme/match)
 (for-syntax   scheme/base))

(provide (all-defined-out))



;; The syntax requires these processing steps:
;;   * Add missing variables
;;   * Add copy clauses
;;   * Sort


;; Convert machine definition form to a symbol-indexed dictionary.
;; Use hash table for usage marking and duplicate checks.

(define (form->clauses form)
  (define hash (make-hash))
  (for-each
   (lambda (clause)
     (match (syntax->list clause)
       ((list-rest name expr)
        (let ((key (syntax->datum name)))
          (when (hash-ref hash key (lambda () #f))
            (raise-syntax-error 'duplicate-name
                                "Form contains duplicate name"
                                clause name))
          (hash-set! hash key clause)))))
   (syntax->list form))
  hash)


(define (clauses-ref/mark-defined! clauses r)
  ;; Hygienically introduce default (identifier not reachable from body code).
  (define (default) (list (datum->syntax #f r)))
  (let ((clause (hash-ref clauses r default)))
    ;; Mark it used.
    (hash-set! clauses r #f)
    clause))

(define (clauses-check-undefined dict)
  (hash-map dict
            (lambda (key notused)
              (when notused
                (raise-syntax-error 'undefined-register
                                    "Undefined register"
                                    notused
                                    (datum->syntax notused key)
                                    )))))

;; Convert machine definition clauses to normal form, completing
;; clauses if necessary, and sorting them in the correct order.
(define (machine-nf registers stx)
  (let* ((dict (form->clauses stx))
         (nf (datum->syntax
              stx
              (for/list ((r registers))
                (syntax-case (clauses-ref/mark-defined! dict r) ()
           
                  ;; Annotated syntax.  This makes it easier to use the same
                  ;; language for clauses with and without pattern matching.
                  ((reg -> expr)        #`(reg reg expr))
                  ((reg : pat -> expr)  #`(reg pat expr))
               
                  ;; Non-annotated.
                  ((reg)          #`(reg reg reg))
                  ((reg pat)      #`(reg pat reg))          
                  ((reg pat expr) #`(reg pat expr))

                  )))))
    (clauses-check-undefined dict)
    nf))
         
       
;; Expand machine instruction.

(define (simple-pattern nf)
  (syntax-case nf ()
    (((reg pat expr) ...)
     (for ((_p (syntax->list #'(pat ...))))
       (let ((p (syntax-e _p)))
         (unless (and (symbol? p))
           (raise-syntax-error 'compound-pattern
                               "Machine expression contains pattern instead of identifier"
                               nf _p))))))
  nf)

;; (define (machine-lambda k registers stx)
;;   (syntax-case (simple-pattern
;;                 (machine-nf registers stx)) ()
;;     (((reg pat expr) ...)
;;      ;; 'reg' can be ignored since body is in normal form (sorted -> positional parameters).
;;      #`(lambda (pat ...) (#,k expr ...)))))

;; (define (machine-match k registers stx)
;;   (syntax-case (machine-nf registers stx) ()
;;     (((reg pat expr) ...)
;;      #`(match (vector reg ...)
;;          ((vector pat ...)
;;           (#,k expr ...))))))

;; (define (machine-match-lambda k registers stx)
;;   (syntax-case (machine-nf registers stx) ()
;;     (((reg pat expr) ...)
;;      #`(lambda (reg ...)
;;          #,(machine-match k registers stx)))))


;; Construct a struct update transformer using the specified register
;; layout.  Note that the names are abitrary: they do not correspond
;; to field names.  That information is gone (only used to construct
;; accessor and mutator names during 'define-struct expansion.
(define (machine-update-struct-tx i struct-id registers-stx stx)
  (let* ((info (extract-struct-info (syntax-local-value struct-id)))
         (make-struct-id (cadr info))
         (size (length (cadddr info)))
         (registers (syntax->datum registers-stx)))
    (when (< size (length registers))
      (raise-syntax-error #f "Too many fields" registers-stx))
    ;; Pad fields if there aren't enough.
    (let ((registers
           (append registers
                   (for/list ((n (in-range (- size (length registers)))))
                     (string->uninterned-symbol (format "R~s" n))))))
      (syntax-case (machine-nf registers stx) ()
        (((reg pat expr) ...)
         #`(match #,i
                  ((struct #,struct-id (pat ...))
                   (#,make-struct-id expr ...))))))))
      


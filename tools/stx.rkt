#lang racket/base
(provide
 (all-defined-out)
 (all-from-out syntax/stx))
(require
 "grabbag.rkt"
 racket/struct-info
 syntax/stx)



(define (format-stx fmt . args)
  (apply format fmt (->sexp args)))
;(define (map-stx fn . stxs)
;  (apply map fn (map syntax->list stxs)))

;; FIXME: doesn't handle all cases (i.e. vectors..)
(define (->sexp x)
  (cond
   ((syntax? x) (syntax->datum x))
   ((list? x)   (map ->sexp x))
   ((pair? x)   (cons (->sexp (car x)) (->sexp (cdr x))))
   ((null? x)   '())
   (else x)))

; (define ->syntax datum->syntax)
; (define ->datum  syntax->datum)


;; This does not recertify!
(define (prefix-id . names)
  (let*
      ((orig-stx
        (car (reverse names))) ;; use original name info
       ;(cci (current-code-inspector))
       (new-stx
        (datum->syntax
         orig-stx   ;; lexical context
         (string->symbol
          (apply string-append
                 (map
                  (lambda (x)
                    (format "~a"
                            (if (syntax? x)
                                (syntax->datum x)
                                x)))
                  names)))
         orig-stx   ;; source info
         orig-stx   ;; properties
         )))
    new-stx))
;    (syntax-recertify ;; NOT EFFECTIVE
;     new-stx orig-stx cci #f)))


(define (stx-reverse stx)
  #`(#,@(reverse (syntax->list stx))))

(define (lexical-binding? stx)
  (eq? 'lexical (identifier-binding stx)))

(define (stx-uncons stx)
  (values (stx-car stx) (stx-cdr stx)))

(define (stx-length s)
  (length (syntax->datum s)))

;; re-interpret lexical interpretation, keeping source location. can
;; be used for 'include' semantics.
(define (lexical-context-from stx-lex)
  (lambda (stx)
    (let ((new-stx
           (datum->syntax stx-lex
                          (syntax->datum stx)
                          stx)))
      ;; (printf "~a ~a ~a\n" (syntax->datum stx) (syntax-source stx) (syntax-source new-stx))
      new-stx)))


;; Expand all sub-expressions in a tree.
;(define-syntax-rule (syntax-case/r tree-stx literals clause ...)
;  (let down ((stx tree-stx))
;    (syntax-case stx literals
;      clause ...
;      ((el (... ...))
;       (map down (syntax->list #'(el (... ...)))))
;      (el #'el))))
      


;; Structs

(define (struct->struct-info name [context name])
  (let ([v (syntax-local-value name (lambda () #f))])
    (unless (struct-info? v)
      (raise-syntax-error #f "identifier is not bound to a structure type" context name))
    (let ((v (extract-struct-info v)))
      (printf "~a\n" v)
      v)))

(define (struct->constructor . a) (cadr (apply struct->struct-info a)))
(define (struct->members . a)     (cadddr (apply struct->struct-info a)))


;; syntax->list drops the certificate on stx.
;; For RPN code this needs to be redistributed.
(define (syntax->list/recertify stx)
  (let* ((cci (current-code-inspector))
         (recert (lambda (stx-new)
                   (syntax-recertify stx-new stx cci #f))))
    (map recert (syntax->list stx))))


(define (in-stx stx) (in-list (syntax->list stx)))


;; Non-higienically introduce a collection of identifiers.
(define (datum->syntax-list stx lst)
  (map (lambda (x) (datum->syntax stx x)) lst))

(define-syntax-rule (syntax-introduce-identifiers stx lst body)
  (syntax-case (datum->syntax-list stx 'lst) ()
    (lst body)))



;; Hash tables of identifiers represented as symbols.  Useful for code
;; analysis.

(define-syntax-rule (define-hashes name ...)
  (begin (define name (make-hash)) ...))


(define (datum id) (if (syntax? id) (syntax->datum id) id))
(define (id-reg! hash id [val #t]) (hash-set! hash (datum id) val))
(define (id-find hash id) (hash-ref hash (datum id) false))
(define (ids hash) (for/list (((k v) hash)) k))


(require (for-template racket/base))

;; Staged macros: safe syntax evaluation at compile time.
;; (require (for-syntax racket/base))
(define-syntax-rule (let-staged ((n v) ...) body ...)
  #`(let-syntax
        ((m (lambda (stx)
              (let ((n v) ...) body ...))))
      (m)))

;; (provide (for-template (all-from-out racket/base)))

    


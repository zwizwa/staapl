#lang scheme/base

;; For the base SCAT language, the running state is a stack,
;; implemented as a list wrapped in a structure type.

(provide

 ;; base object
 state-lambda  update
 state-update      

 ;; stack
 stack
 stack-list
 stack-cons
 stack-uncons
 stack-top
 stack-lambda

 make-state:stack
 state:stack

 scat-dwim
 scat-wrap-dynamic
 
 )

(require
 "../rpn.ss"
 scheme/match
 scheme/stxparam
 (for-syntax
  "../ns-tx.ss"
  scheme/pretty
  scheme/base))

;; STATE Inheritance & functional update.

;; Read is handled by structure type inheritance. Write is implemented
;; through a factory method stored in the first 'update' field.

(define-struct state (update))  

(define-syntax-parameter update
  (lambda (stx)
    (raise-syntax-error #f "can only be used inside `state-lambda'" stx)))

;; Matcher for derived structures. Non-hygienically introduces 'update'.
;; FIXME: use syntax parameters (this guesses symbol context from state-type)
(define-syntax (state-lambda stx)
  (syntax-case stx ()
    ((_ state-type (var ...) . expr)
     ;; (pretty-print (syntax->datum stx))
     #`(lambda (state)
         (match state
                ((struct state-type (update-fn var ...))
                 (let ((_update
                        (lambda args
                          (apply update-fn state args))))
                   (syntax-parameterize
                    ((update (make-rename-transformer #'_update)))
                    . expr)))
                (else
                 (error 'state-lambda "match failed for ~s, wanted type ~s"
                        state 'state-type)))))))


;; STACK object

;; Parameter stack = state object for the SCAT language. This is
;; extended with extra state for the MACRO language.

(define-struct (stack state) (list))  

(define (make-state:stack l)
  (let ((update ;; for object-name
         (lambda (state lst) (make-state:stack lst))))
    (make-stack update l)))

;; Type values are empty constructors.
(define (state:stack)
  (make-state:stack '()))
  

;; Stack update.
(define (stack-cons a s)
  ((state-lambda stack (l)
                 (update (cons a l)))
   s))
   
(define stack-uncons
  (state-lambda stack
                (stack)
                (unless (pair? stack)
                  (error 'stack-underflow))
                (values
                 (car stack)
                 (update (cdr stack)))))

;; Lift functions that operate on the scheme argument list to
;; functions operating on the stack struct.
(define-syntax stack-lambda
  (syntax-rules ()
    ((_ formals . body)
     (state-lambda stack
                   (stack)
                   (update
                    (apply (lambda formals . body) stack))))))

(define (stack-top s)
  (let-values
      (((top rest) (stack-uncons s)))
    top))



;; Dynamic scheme wrapping.  This is used in the interactive Forth
;; console to provide "do-what-i-mean" behaviour.

(define (scat-wrap-dynamic fn)
  (unless (procedure? fn)
    (error 'scat-wrap-dynamic "not a procedure: ~a\n" fn))
  (state-lambda stack
                (stack)
                (update
                 (apply (rpn-wrap-dynamic fn) stack))))

(define-syntax (scat-dwim stx)
  (syntax-case stx ()
    ((_ id)
     (let ((pid (ns-prefixed #'(scat) #'id)))
       (if (identifier-binding pid)
           pid
           #'(scat-wrap-dynamic id))))))
         

  

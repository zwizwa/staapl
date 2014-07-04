#lang racket/base

(provide
 (all-defined-out))

(require
 "rpn-scat.rkt"
 "scat-base.rkt"
 "stack.rkt"
 "../ns.rkt"
 "rep.rkt"
 
 "base-utils.rkt"

 "../tools.rkt"
 "scat-syntax.rkt"

 racket/match
 racket/control

 (for-syntax
  syntax/stx
  racket/base)
 
 )

;; Control operations for the SCAT base language: anything that needs
;; to grab the whole state. In contrast with the functions in
;; scat-base, which operate only on the stack, these need to know
;; about the state object.


;; The passing of threaded state is abstracted in the functions below.

(define-syntax define-ctrl
  (syntax-rules ()
     ((_ name . args)
     (ns (scat) (define name
                  (ctrl-lambda . args))))))

;; Abstract implementation of stack and hidden data.
;; (define-syntax (ctrl-lambda stx)
;;   (syntax-case stx ()
;;     ((_ (proto ...) . expr)
;;      #`(lambda (state)
;;          (let ((#,(datum->syntax (stx-car #'expr) 'stack->state)
;;                 (lambda (stack)
;;                   (update-state state stack))))
;;            #,(syntax-case #'(proto ...) ()
;;                ((s t u ...)
;;                 #`(match state
;;                          ((struct stack (ctor (list-rest . (s t u ...))))
;;                           . expr)))
;;                ((stack)
;;                 #`(match state
;;                          ((struct stack (ctor stack))
;;                           . expr)))))))))

(define-syntax (ctrl-lambda stx)
  (syntax-case stx ()
    ((_ (s t u ...) . expr)
     #`(state-lambda #,(datum->syntax #'s 'stack)  ;; context for 'update'
                     ((list-rest . (s t u ...)))
                     ;; =>
                     . expr))
    ((_ (stack) . expr)
     #`(state-lambda #,(datum->syntax #'s 'stack)  ;; context for 'update'
                     (stack)
                     ;; =>
                     . expr))))

;; Attempt to write a macro that prevents functions to ever access the
;; state data. It looks like it's easier to just have an explicit
;; parameter and state accessors + knowledge of implementation of
;; functions.

;; (define-syntax (broem-lambda stx)
;;   (syntax-case stx ()
;;     ((_ stack . expr)
;;      #`(control-lambda
;;         stack data
;;         (let ((#,(syntax->datum stx 'STATE) ;; introduce name
;;                (lambda (s) (make-state s data))))
;;           . expr)))))
        



;; CONTROL WORDS

;; These use the following functions to access/create state:
    
;;   * state->stack   (extracts stack from state)
;;   * stack->state   (combines stack with enclosing state)
;;   * state-cons     (push element to stack in state)

(define state->stack stack-list)
(define state-cons   stack-cons)
(define state-top    stack-top)

;; Function application.

(define-ctrl run (fn stack+)
  (fn (update stack+)))


;; Run, but preserve top element.
(define-ctrl dip (fn datum stack+) 
  (state-cons datum (fn (update stack+))))

;; Run a function in a modified environment. See scat-base.ss for
;; dynamic environment constructors.
(define-ctrl dynamic (consume-thunk fn stack+)
  (consume-thunk
   (lambda ()
     (fn (update stack+)))))


;; call/cc     (fn -- )

;; Push the current continuation on the stack, wrapped in state
;; function, and execute fn. The continuation wrapper when invoked,
;; passes the whole state.

(define-ctrl call/cc (fn stack+)
  (call/cc
   (lambda (k)
     (fn (update
          (cons
           (lambda (s) (k s))
           stack+))))))



;; amb-choose  (now later save -- now/later)

;; Non-deterministic choice: Create a continuation which backtracks
;; and invokes the 'later' branch. Pass this continuation to and
;; passes it to 'save', then returning the the 'now' branch.

;; amb :: (amb-choose run)

(define-ctrl amb-choose (save later now stack+)
  (call/cc
   (lambda (k)
     (state-cons
      now
      (save (update
             (cons
              (lambda _ (k (update (cons later stack+))))
              stack+)))))))


;; Similar, but using prompts. Go through a series of alternatives.

;; does it drop or not? -> no: it's usually a check inserted after
;; something that returns a value or false.

(define attempt-tag
  (make-continuation-prompt-tag 'attempt))

(define-word check s
  (if (car s)
      s (abort-current-continuation
         attempt-tag (lambda () #f)))) 

(define-ctrl attempts (alternatives stack+)
  (let next ((a alternatives))
    (if (null? a)
        (error 'attempts-exhausted)
        (or
         (prompt-at attempt-tag ((car a) (update stack+)))
         (next (cdr a))))))
        


;; Map will leave the rest of the stack and state data untouched:
;; mapped functions get it passed, but the modification is ignored:
;; only the top stack element is collected.

(define-ctrl map (fn lst stack+) 
  (update
   (cons
    (map
     (lambda (x)
       (state-top
        (fn (update (cons x stack+)))))
     lst)
    stack+)))

;; A function that behaves like 'map', but operates on a list of
;; stacks.

(define-ctrl stack-map (fn stacks stack+)
  (update
   (cons
    (map (lambda (stack)
           (state->stack (fn (update stack))))
         stacks)
    stack+)))




;; For-each is list interpretation. For this reason, the last function
;; is evaluated in tail position. (see 'interpret-list').

(define-ctrl for-each (fn l stack+)
  (interpret-list (lambda (i s)
                    (fn (state-cons i s)))
                  car cdr null?
                  l (update stack+)))


(define-ctrl for (fn n stack+)
  (when (negative? n)
    (error 'negative-index "~a" n))
  (interpret-list (lambda (_ s) (fn s))
                  void sub1 zero?
                  n (update stack+)))


;; Left and right fold. The prototype is analogous to that of
;; for-each, which is related to left fold.

(define (make-fold fold)
  (ctrl-lambda (fn l i stack+)
    (update
     (cons
      (fold
       (lambda (kar kdr)
         (state-top
          (fn (update
               (list* kdr kar stack+)))))
       i l)
      stack+))))
  
(ns (scat) (define foldl (make-fold foldl)))
(ns (scat) (define foldr (make-fold foldr)))



;; Composition

;; These are here because they know about the code representation
;; (unary scheme functions).

(define-word compose (g f . stack+)
  (cons
   (make-word
    (lambda (state)
      (g (f state))))
   stack+))
   

;; Partial application (conspose). The closest thing to closures. I
;; feel no need for 'quote' as in Joy. Just use "() cns". Here it is
;; possible to relativly unambiguously create a source rep.

(define-word cns (f a . stack+)
  (cons
   (make-word
    (lambda (state)
      (f (state-cons a state))))
   stack+))


;; Exception handling using 'catch'. If the 'body' code generates an
;; exception that is tagged with 'tag', the 'handler' code is
;; invoked with the exception value pushed to the stack.

;; (define-word catch (tag handler body . s)
;;   (with-handlers
;;       (((lambda (ex)
;;           (or (eq? tag #t) ;; catch all
;;               (and (list? ex)
;;                    (eq? tag (car ex)))))
;;         (lambda (ex)
;;           (apply-base (code handler)
;;                       (cons ex ;; (pretty-exn ex)
;;                             s)))))
;;     (apply-base (code body) s)))


;; Fixme: error capture disabled here: handle somewhere else.
;(define-word run/error (fn . s)
;  (apply-base (code fn) s))




;; Apply a function to an isolated stack, ignoring state WRITE effects.
(define-ctrl apply (i-stack fn stack+)
  (let ((o-stack
         (state->stack
          (fn (update i-stack)))))
    (update (cons o-stack stack+))))


;; these compositions are 'tainted' with control flow.

(compositions
 (scat) scat:

 (2dip      (cons) dip dip uncons)
 ;; (p-error  "\nERROR:" d d cr)
 (pl       (p cr) for-each)

 
 (list       '() apply reverse)

 
 ;; flow control
 (ifte      choose run)
 (if        () ifte)
 (unless    () swap ifte)
;; (try       #t catch)        ;; catch all exceptions
 (forever   dup dip forever) ;; loop forever (until exception)
;; (need      over (drop) (throw) ifte)
 
;; (1-throw   ('() cons) dip swons throw)
 (2run      (run) dip run)
 
 ;; file
;; (try-delete-file  (delete-file) (drop drop) try)
 

 
       
 )

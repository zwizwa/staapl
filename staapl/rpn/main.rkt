#lang racket/base




(require "parse.rkt"
         "../ns.rkt"
         "parse-tx.rkt"
         (for-template
          racket/base)
         (for-syntax
          "../tools/io.rkt"
          "../tools/stx.rkt"
          "parse-tx.rkt"
          racket/base))

(provide
 (all-defined-out)
 (all-from-out "parse.rkt")
 (all-from-out "parse-tx.rkt"))



;; A simple straight line -> nested code transformer producing a
;; lambda expression.

;; This form is used in almost all rpn-based languages.  It takes a
;; list of (form expr) forms and performs a nested expression by
;; creating an expression (from expr p <nest>).  This can then be used
;; to create a ``nested let'' expression, which produces
;; single-assigment code.

(define-syntax (rpn-lambda stx)
  (syntax-case stx ()
    ((_ . txs)
     #`(lambda (p)
         #,(foldr (lambda (compile expr)
                    (append (syntax->list compile)
                            (list #'p expr)))
                  #'p (syntax->list #'txs))))))


;; Used to pass the dictionary containing a single anonymous entry to
;; a form like rpn-lambda.

(define-syntax rpn:-compile
  (syntax-rules ()
    ((_ (compile code ...)) ;; single entry
     (compile code ...))))




;; For console interaction: pass a lexed string to a compiler macro.
(define-syntax (rpn-lex stx)  
  (syntax-case stx ()
    ((_ compile str)
     (let ((words
            (port->syntax-list
             (open-input-string (syntax->datum #'str)) stx)))
       #`(compile #,@words)))))



;; With a nested let representation, local values are straightforward
;; to implement.  Values are popped off the stack and bound to
;; intermediate scheme variables.  Then for each variable a wrapper
;; word is created in the proper namespace.  These words load the
;; respective value on the stack.

(define-syntax (rpn-let-locals stx)
  (syntax-case stx ()
    ((_ (namespace
         program:
         pop-values)
        (formal ...) p sub)
     (let ((flist (syntax->list #'(formal ...))))
       #`(let-values (((p formal ...) (pop-values p #,(length flist))))
           (ns namespace
               (let ((formal (program: ',formal)) ...)
                 sub)))))))



;; Scheme snarfing

;; Convert a scheme function to RPN form based on 'procedure-arity.
;; Note that this is a dynamic wrapper.  I'm do not know if it is much
;; faster to do this at syntax-time.  The resulting function operates
;; on the argument list as a stack.  For functions with have an
;; arity-at-least struct, the optional arguments are wrapped as a
;; list.  Keywords are obviously not supported.

;; Note:
;;
;;  * automatic syntax-based snarfing is not possible in scheme
;;    without some tricks that provide an instantiated module at
;;    compile time for inspection.
;;
;;  * return values of functions are unknown.  we can dynamicly
;;    capture multiple values though.


(define (rpn-take-reversed n lst)
  (let _take ((n n)
              (in lst)
              (out '()))
    (if (or (zero? n)
            (null? lst))
        (values out in)  
        (_take (sub1 n)
               (cdr in)
               (cons (car in) out)))))

(define (rpn-apply->list fn args)
  (let ((args/void
         (call-with-values
             (lambda () (apply fn args))
           list)))
    (if (void? (car args/void))
        '()
        args/void)))
  
;; Note: this function performs reasonable guesses.  It's a dwim
;; feature.
(define (rpn-wrap-dynamic fn)
  ;; (printf "rpn-wrap-dynamic: ~s ~s\n" fn (procedure-arity fn))
  (lambda stack
    ;; (printf "(rpn-wrap-dynamic) stack = ~s" stack)
    (define (go n stack [optional '()])
      (let-values (((args stack+) (rpn-take-reversed n stack)))
        (append
         (rpn-apply->list fn (append args optional))
         stack+)))
    (define (dispatch n)
      (cond
       ((arity-at-least? n)
        (go (arity-at-least-value n) (cdr stack) (car stack)))
       ((number? n) (go n stack))
       ((list? n) (dispatch (car n))) ;; take first
       (else (error 'rpn-wrap-dynamic "~a" n))))
    (dispatch (procedure-arity fn))))


;; Doesn't support multiple values.
(define-syntax (rpn-wrap-static stx)
  (syntax-case stx ()
    ((_ nargs fn)
     (let ((formals
            (generate-temporaries
             (build-list (syntax->datum #'nargs) add1))))
       #`(lambda (#,@(reverse formals) . stack)
           (cons (fn #,@formals) stack))))))
    
  
;; Might be useful in ordinary scheme code too..
(define (rpn-wrap: . fns)
  (lambda stack
    (foldl apply
           stack
           (map rpn-wrap-dynamic fns))))

;; Useful for dumping symbolic code into a hash table. Used in
;; standalone DTC Forth bootstrapper.
(define-syntax-rule (rpn-register-entry reg!)
  (syntax-rules ()
    ((_ #f _ ) (void))
    ((_ #f _ . _) (syntax-error))
    ((_ name compile . code)
     (reg! 'name (compile . code)))))

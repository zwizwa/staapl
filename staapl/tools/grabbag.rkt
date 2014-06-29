#lang racket/base



(provide
 (all-defined-out))

(define (msleep ms)
  (sync (alarm-evt
         (+ (current-inexact-milliseconds)
            ms))))


;; Convert anything to a port.
(define (port string/port)
  (cond
   ((string? string/port)
    (open-input-string
     (string-append string/port "\n")))
   ((port? string/port)
    string/port)
   (else
    (error 'invalid-type string/port))))



(define (make-counter init)
  (let ((state (- init 1)))
    (lambda ()
      (set! state (+ 1 state))
      state)))

(define (id . vals) (apply values vals))
(define (true . args) #t)
(define (false . args) #f)

(define-syntax-rule (fail/false expr ...)
  (with-handlers ((void false)) expr ...))


(define-syntax-rule (inc! val) (begin (set! val (add1 val)) val))

(define (resolve-module m)
  ((current-module-name-resolver) m #f #f #f))


;; Trace return value
(define-syntax-rule (*** fn arg ...)
  (let ((rv (fn arg ...)))
    (printf "*** ~s\n" rv)
    rv))


;; Label symbol generator.
(define next-label
  (let ((next (make-counter 0)))
    (lambda () (string->symbol
                (format ".L~a" (next))))))

;; From PLT list
(define (definition-source id)
  (let ([binding (identifier-binding id)])
    (and (list? binding)
         (resolved-module-path-name
          (module-path-index-resolve (car binding))))))

;; Push to a parameter stack.
(define (ppush! param val [error (lambda () (error 'push-pstack-undefined))])
  (let ((stack (param)))
    (unless stack (error))
    (param (cons val stack))))

;; Inlined quote.
(define-syntax-rule (quote* . a) (quote a))
(define-syntax-rule (quasiquote* . a) (quasiquote a))


(require racket/runtime-path)
(define-runtime-path home-dir "..")
(define (home) (simplify-path home-dir))

(require racket/match)
(require racket/pretty)
(define (pretty-expand form [expand expand])
  (let ((expr (syntax->datum (expand form))))
    (pretty-print
     (let cleanup ((expr expr))
       (match expr
              ((cons '#%top var) var)
              ((list '#%expression expr) (cleanup expr))
              ((cons '#%app expr) (cleanup expr))
              (else
               (if (list? expr)
                   (map cleanup expr)
                   expr)))))))



;; Garbage collector:

;; IN:  dictionary:  obj -> (listof obj)
;;      root:        (listof obj)
;; OUT: live / dead: (listof obj)

;; Dictionary is specified in terms of a function.

;; http://blog.plt-scheme.org/2008/02/dirty-looking-hygiene.html
(require racket/stxparam
         (for-syntax racket/base))
(define-syntax-parameter it
  (lambda (stx)
    (raise-syntax-error #f "can only be used inside `if*'" stx)))
(define-syntax if*
  (syntax-rules ()
    [(if*) (void)]
    [(if* X) X]
    [(if* C X more ...)
     (let ([b C])
       (if b
           (syntax-parameterize ([it (make-rename-transformer #'b)]) X)
           (if* more ...)))]))


;; Compute a live set of objects from a collection of root objects and
;; an a map relating an object to its immediate dependencies.
(define (dependencies deps root)
  (define live (make-hash))
  (define (mark! obj) (hash-set! live obj #t))
  (define (mark? obj) (hash-ref live obj false))
  (define (mark-deps! obj)
    (unless (mark? obj)
      (mark! obj)
      (if* (deps obj) (for ((o it)) (mark-deps! o)))))
  (for ((r root)) (mark-deps! r))
  (for/list (((k v) live)) k))
    


;; Add support for a collection of easier to read define-syntax-rule
;; forms implemented in terms of the `define-syntaxes' signature form.
(require racket/unit)
(define-syntax define-signature+
  (syntax-rules (define-syntax-rule_)
    ((_ sig^ (sig-expr ...) ;; standard define-signature forms
        (define-syntax-rule_ (name . args) rule) ...) ;; "plus" version
     (define-signature sig^
       (sig-expr ...
        (define-syntaxes (name ...)
          (values (syntax-rules () ((_ . args) rule)) ...)))))))

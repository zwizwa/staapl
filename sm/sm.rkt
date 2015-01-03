#lang racket/base
(require racket/dict)

;; Non-recursive state machine languages has:
;;  - primitives
;;  - returning non-tail calls (function abstraction)
;;  - non-returning tail calls (a program never returns)
;;  - yield for task switching
;;
;; Main tricks:
;;  - all "lexical" variable are global statics
;;  - all non-tail calls are inlined
;;  - tail calls are "gotos with arguments"
;;  - yield is trivial: save current instruction pointer
;;
;; Later optimizations:
;; - register allocation to reuse memory cells
;; - use run-time stack to implement non-yielding functions used multiple times
;;
;; TODO:
;; - conditionals

(define nb-vars      (make-parameter #f))
(define code         (make-parameter #f))
(define functions    (make-parameter #f))
(define stack-depth  (make-parameter #f))
  
(define (with-context thunk)  
  (parameterize ((nb-vars 0)
                 (code '())
                 (functions '())
                 (stack-depth 0)) ;; to prevent infinite recursion
    (thunk)))
    
(define (alloc-variable!)
  (let ((rv (nb-vars)))
    (nb-vars (add1 rv))
    rv))
(define (compile-statement! s)
  (code (cons s (code))))
(define (register-function! name vars code)
  (functions (cons (list name vars code) (functions))))

(define *count* 0)
(define (check-count)
  (set! *count* (add1 *count*))
  (when (> *count* 10)
    (raise '*count*)))

(define (compile-function! name body)
  (check-count)
  (unless (dict-ref (functions) name (lambda () #f))
    (let* ((nb (procedure-arity body))
           (arg-vars (for/list ((n nb)) (alloc-variable!)))
           (code-box (box #f)))
      ;; Create entry before compilation to allow for recursive calls.
      (register-function! name arg-vars code-box)
      (parameterize ((code '()))
        ;; compile function and save the body code.
        (apply body (for/list ((v arg-vars)) `(ref ,v)))
        (set-box! code-box (reverse (code))))))
  
  ;; Return input variables
  (car (dict-ref (functions) name)))
  
      

;; Declaring a variable allocates a unique storage location and adds
;; it to the current environ.
(define-syntax-rule (_let1 name val body)
  (let ((var (alloc-variable!)))  
    (compile-statement! `(set! ,var ,val))
    (let ((name `(ref ,var))) body)))

(define-syntax _let*
  (syntax-rules ()
    ((_ () b) b)
    ((_ ((n v) . nvs) b)
     (_let1 n v (_let* nvs b)))))

    
(define-syntax-rule (_prim op . args)
  `(,'op ,@(list . args)))

(define-syntax-rule (_const c)
  `(const ,'c))

;; All functions are named
(define-syntax-rule (_def (name . args) body)
  (define (name . args) body))

;; Application:
;;  - non tail call just inlines the function
(define-syntax-rule (_apply_ntc fn . args)
  (parameterize ((stack-depth (add1 (stack-depth))))
    (when (> (stack-depth) 10)
      (raise 'infinite-ntc-recursion))
    (apply fn (list . args))))

;;  - tail call compiles function + label if it's not yet compiled,
;;  and inserts a "goto with arguments".
(define-syntax-rule (_apply_tc  fn . args)
  (begin
    (unless (zero? (stack-depth))
      (raise 'non-toplevel-tailcall))
    (let ((vars (compile-function! 'fn fn)))  ;; compile function if not already compiled
      (for ((v vars)
            (a (list . args)))
        (compile-statement! `(set! ,v ,a))))
    (compile-statement! `(goto ,'fn))))
  

;; Test

;; Used as inlined ntc functions.
(define (op1 a b) (_prim + a b))
(define (op2 a b) (_prim * a b))

;; Used as mutually recursive tc functions.
(define (state1 acc inc)
  (_let* ((acc+ (_apply_ntc op1 acc inc)))
      (_apply_tc state2 acc+ inc)))
(define (state2 acc inc)
  (_let* ((acc+ (_apply_ntc op2 acc inc)))
      (_apply_tc state1 acc+ inc)))

(define-syntax-rule (compile-machine name)
  (with-context
   (lambda ()
     `((input . ,(compile-function! 'name name))
       (nb-vars . ,(nb-vars))
       (functions . ,(map unbox-function (reverse (functions))))))))

(define (unbox-function f)
  (list `(name . ,(list-ref f 0))
        `(args . ,(list-ref f 1))
        `(code . ,(unbox (list-ref f 2)))))

(compile-machine state1)

;; Practically useful machine: a UART.

;; This consists of two state machines "clocked" at different rates.
;; There is the machine that listens to the start bit transition and
;; subsequently calls the shift register at the reduced clock rate.

;; To handle input we use erlang-style messages.  Synchronous at first
;; and later possibly a-synchronous.

;; Don't implement pattern matching.  Keep it simple at first: input
;; structure is static.



;; (define (uart-start prev-in)
;;   (_let* ((in (_receive)))
;;       (_if (= in prev-in)
;;          (uart-start in)
;;          (uart-shift baud
;;                      (+ baud
;;                         (>> baud 2))
;;                      8
;;                      0))))

;; (define (uart-shift baud tick bits sr)
  
;;   (_let* ((in (_receive))) ;; 
;;   (_if (= tick 0)
       
         
    
  
  





;; Register allocation: at each point in the code, it's clear what
;; variables are accessible.  This should be enough to serve as input
;; to a register allocator.

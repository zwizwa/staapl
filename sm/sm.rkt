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


(define nb-vars   (make-parameter #f))
(define code      (make-parameter #f))
(define environ   (make-parameter #f))
(define functions (make-parameter #f))
  
(define (with-context thunk)  
  (parameterize ((nb-vars 0)
                 (code '())
                 (environ '())
                 (functions '()))
    (thunk)))
    
(define (alloc-variable!) (let ((rv (nb-vars))) (nb-vars (add1 rv)) rv))
(define (compile-statement! s) (code (cons s (code))))
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
      (parameterize
          ((code '())
           (environ (append arg-vars environ)))
        ;; compile function and save the body code.
        (apply body (for/list ((v arg-vars)) `(ref ,v)))
        (set-box! code-box (reverse (code))))))
  
  ;; Return input variables
  (car (dict-ref (functions) name)))
  
      

;; Declaring a variable allocates a unique storage location and adds
;; it to the current environ.
(define-syntax-rule (_let1 name val body)
  (let ((var (alloc-variable!)))  
  (printf "functions: ~a\n" (functions))

    (parameterize ((environ (cons var (environ))))
      (compile-statement! `(set! ,var ,val))
      (let ((name `(ref ,var)))
        body))))
    
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
  (fn args))
;;  - tail call compiles function + label if it's not yet compiled,
;;  and inserts a "goto with arguments".
(define-syntax-rule (_apply_tc  fn . args)
  (begin
    (let ((vars (compile-function! 'fn fn)))  ;; compile function if not already compiled
      (for ((v vars)
            (a (list . args)))
        (compile-statement! `(set! ,v ,a))))
    (compile-statement! `(goto ,'fn))))
  

;; Test

;; Mutually recursive functions.
(define (state1 acc inc)
  (_let1 acc+
         (_prim + acc inc)
         (_apply_tc state2 acc+ inc)))
(define (state2 acc inc)
  (_let1 acc+
         (_prim + acc inc)
         (_apply_tc state1 acc+ inc)))

(define-syntax-rule (compile-machine name)
  (with-context
   (lambda ()
     `((input . ,(compile-function! 'name name))
       (nb-vars . ,(nb-vars))
       (functions . ,(functions))))))

(compile-machine state1)



;; Register allocation: at each point in the code, it's clear what
;; variables are accessible.  This should be enough to serve as input
;; to a register allocator.

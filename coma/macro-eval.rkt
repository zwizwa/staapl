#lang racket/base

;; Tools for accessing the code stack useful for macro evaluation.

(require
 "../op.rkt"
 "../ns.rkt"
 "../scat.rkt"
 "../target/rep.rkt"
 "op.rkt"
 racket/match)

(provide

 state->value
 state->code
 
 asm-pop
 state-pop

 insert
 
 state-print-code

 tag-stack

 macro-list->state
 state->macro-list


 macro->target-word
 macro-target-word?
 )

;; Evaluate macro to expose the target word call it wraps.  Will raise
;; 'invalid-argument when it's not a wrapping macro.
(define (macro->target-word x)
  (unless (word? x)
    (error 'macro->target-word "~s" x))
  (let ((v
         (state->value
          (x (state:stack))
          (ns (op ? cw)))))
    v))
(define (macro-target-word? m)
  (with-handlers ((void (lambda _ #f)))
    (macro->target-word m)
    #t))
    


(define (state-print-code state)
  (for ((ins (map instruction->string
                  (reverse (stack-list state)))))
    (display ins)
    (newline)))


;; *** COMPILATION TOOLS ****

(define (tag-stack s tag)
  (map (lambda (v) (list tag v)) s))

(define state->code stack-list)

;; Obtain single tagged value from state.  Used in macro evaluation.
(define (state->value state tag?)
  (let-values
      (((asm vals)
        (asm-pop (stack-list state) 1 tag?)))
    (unless (null? asm)
      (error 'multiple-asm-values "~s" asm))
    (car vals)))


;; State construction is abstracted, in case there are macros operate
;; on extended state.

;(define macro-eval-init-state
;  (make-parameter
;   (lambda ([s '()]) (make-state:stack s))))


;; POP & UNQUOTE
;; access tagged values on the stack

;; nvals = negative -> pop all

(define (asm-pop in-instructions nvals tag?
                 [trouble #f])
  (let loop ((ins  in-instructions)
             (n    nvals)
             (vals '()))
    (if (or
         (zero? n)
         (and (< n 0) (null? ins)))
        (values ins vals)
        (begin
          (when (null? ins)
            (if trouble (trouble)
                (error 'asm-pop-stack-underflow
                       "~s: ~s" nvals in-instructions)))
          (let ((op (car ins)))
            (unless
                (and
                 (pair? op)
                 (tag? (car op))
                 (pair? (cdr op)))
              (if trouble (trouble)
                  (error 'invalid-argument
                         "~a" (cons (asm-name (car op)) (cdr op)))))
            (loop (cdr ins)
                  (- n 1)
                  (cons (cadr op) vals)))))))

(define (state-pop state nvals tag?)
  ;; The abstract state update machanism has no way to split data off
  ;; state, so use local (single) assignment.
  (define popped-values #f)
  (define state+
    ((state-lambda stack
                   (asm)
                   (update
                    (let-values
                        (((asm+ vals)
                          (asm-pop asm nvals tag?)))
                      (set! popped-values vals)
                      asm+)))
     state))
  (values state+ popped-values))


(define (insert instructions)
  (make-word
   (state-lambda stack
                 (asm)
                 (update (append (reverse instructions) asm)))))


;; Convert a list of macros to a state by running them one by one.
(define (macro-list->state macro-list make-state)
  (foldl (lambda (macro state) (macro state))
         (make-state)
         macro-list))

;; Convert a state (a list of instructions) to a list of macros that
;; produce the instructions one by one).
(define (state->macro-list state)
  (map (lambda (x) (scat: ',x)) ;; quote the whole instruction
       (reverse (state->code state))))


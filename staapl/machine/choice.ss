#lang scheme/base

;; Backtracking (relational programming) with the following features:

;;  - composable (using partial continuations)
;;  - allows for dynamic parameters to store (functional) threaded state
;;  - enumerator interface for choices and solutions

(provide query/parameterize
         query
         solutions  ;; produces an enumerator
         choice choice/mark choice/enum
         choice/fail
         cut)

(require "enum.ss"
         "fail.ss"
         srfi/41
         scheme/control
         scheme/match)

;; SHIFT + PARAMETER SAVE / RESTORE

(define choice-tag (new-prompt))

(define-syntax shift/parameters
  (syntax-rules ()
    ((_ () k expr)
     (shift-at choice-tag k expr))
    ((_ parameters k expr)
     (let ((ps (list . parameters)))
       (apply (lambda (x . pvs)
                (for ((p ps) (pv pvs)) (p pv))
                x)
              (let ((pvs (for/list ((p ps)) (p))))
                (shift-at choice-tag _k
                          (let ((k (lambda (x) (_k (cons x pvs)))))
                            expr))))))))

;; CONTROL OPS 

;; The driver uses 2 instructions to sequence search, which either
;; instruct contexts to be saved or restored.

;; A `choicepoint' abstracts a fork in the search tree as a
;; combination of the current path, represented as a partial
;; contiuation with saved dynamic state, and its branch points,
;; abstracted by an enumerator.

(define-struct choicepoint (k enum mark?))

;; A `result' abstracts the end of a search path, combined with
;; instructions to retain a value as a solution, and whether to cut
;; the search tree to the currently marked point.

(define-struct result (cut? solution? value))

;; Enumerate solutions to nondeterminisitc program as a left fold.
;; Similarly, choices are specified using an enumerator.  This allows
;; straightforward composition of multiple searches.

;; ( Note that inside this routine, we explicitly loop, turning all
;; choice superfolds into streams.  It's probably possible to simply
;; compose the enumerators as function instead of appending streams.
;; I find streams in this instance easier to understand though.. )


(define (no-driver . _) (error 'choice-no-driver))
(define current-choice (make-parameter no-driver))

(define (choice . vals) ((current-choice) (list->enum vals) #f))
(define (choice/mark . vals) ((current-choice) (list->enum vals) #t))
(define (choice/enum enum [mark? #f]) ((current-choice) enum mark?))
(define (choice/fail . _) (abort-current-continuation
                           choice-tag (lambda () (make-result #f #f #f))))

(define (cut . lst)
  (abort-current-continuation
   choice-tag (lambda ()
                (cond
                 ((null? lst) (make-result #t #f #f))
                 ((null? (cdr lst)) (make-result #t #t (car lst)))
                 (else 'cut-nargs "~a" lst)))))


;; DRIVER LOOP

(define-struct choice-prog (thunk))
(define (choice-foldl/list fn init program [breadth-first? #f])
  (define _cut_ (stream (lambda () (make-result #t #f #f))))
  (let next ((paths (stream (choice-prog-thunk program)))
             (marks '())
             (state init))
    (if (stream-null? paths)
        state ;; done
        (let ((resume (stream-car paths))
              (paths- (stream-cdr paths)))
          (match (resume)

                 ;; Push choice points to paths, possibly marking
                 ;; current paths for `cut'.
                 ((struct choicepoint (k enum mark?))
                  (let ((thunks (stream-map
                                 (lambda (val)
                                   (lambda () (k val)))
                                 (enum->stream enum))))
                    (if breadth-first?
                        (if (not mark?)
                            (next (stream-append thunks paths-) marks state)
                            (error 'breadth-first-with-mark))
                        (if (not mark?)
                            (next (stream-append paths- thunks) marks state)
                            (next
                             (stream-append thunks _cut_) ;; pop marks when done
                             (cons paths- marks)  ;; push current paths on mark stack
                             state)))))

                 ;; Update fold state if there's a result, handle `fn'
                 ;; termination requests, otherwise continue search,
                 ;; possibly pruning tree.
                 ((struct result (cut? solution? value))
                  (when (and cut? (null? marks))
                    (error 'cut-without-marks))
                  (let-values
                      (((cont? state)
                        (if solution?
                            (call-with-values
                                (lambda () (apply fn value state))
                              (lambda (cont? . state+)
                                (values cont? state+)))
                            (values #t state)))
                       ((paths marks)
                        (if cut?
                            (values (car marks) (cdr marks))
                            (values paths- marks))))
                    (if cont?
                        (next paths marks state)
                        state))) ;; done
                            
                 (ins (error 'choice-foldl "invalid instruction: ~a" ins)))))))

;; Create the solutions enumerator.
(define (solutions prompt-thunk [breadth-first #f])
  (lambda (fn . init)
    (apply values (choice-foldl/list fn init prompt-thunk breadth-first))))


;; Create a non-deterministic program context.
(define-syntax-rule (query/parameterize ((p pv) ...) . body)
  (make-choice-prog
   (lambda ()
     (prompt-at
      choice-tag
      (parameterize
          ((current-fail choice/fail)
           (current-choice
            (lambda (enum mark?)
              (shift/parameters
               (p ...) k
               (make-choicepoint k enum mark?))))
           (p pv) ...)
        (make-result #f #t  ;; don't cut, use result
                     (begin . body)))))))

(define-syntax-rule (query . body)
  (query/parameterize () . body))


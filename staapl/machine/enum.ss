#lang scheme/base

;; In [1][2] enumerators in the form of left folds with premature
;; termination are presented as the superior traversal interface.
;; (a.k.a. `superfold' [5]).  This module provides some utility
;; functions linking them with lists, lazy lists and PLT Scheme
;; sequences.
;;
;; An enumerator is a left-fold-like function
;;   (fn init1 init2 ...)    -> (final1 final2 ...)
;;
;; with `fn' a state update function
;;   (el state1 state2 ...) -> (cont? state1+ state2+ ...)
;;
;; where `cont?' indicates continue/terminate and the states are
;; chained (the next invokation of `fn' take the results of the
;; previous one.)

;; [1] http://lambda-the-ultimate.org/node/1224
;; [2] http://okmij.org/ftp/Streams.html#enumerator-stream
;; [3] http://srfi.schemers.org/srfi-41/srfi-41.html
;; [4] http://srfi.schemers.org/srfi-45/srfi-45.html
;; [5] http://www.eros-os.org/pipermail/e-lang/2004-March/009643.html

(require scheme/base)
(require scheme/control)
(require srfi/45)
(require srfi/41)

(provide (all-defined-out))

;; loose termination
(define (fn/fold->fn/enum fn)
  (lambda args
    (call-with-values
        (lambda () (apply fn args))
      (lambda args
        (apply values #t args)))))

(define (enum->stack enum) (enum (fn/fold->fn/enum cons) '()))
(define (enum->list enum) (reverse (enum->stack enum)))

;; (*) Note that if (not cont?) we need to return the `state+'
;; returned by `fn' and not the `state' passed to it.  Otherwise `fn'
;; can't influence the final state.

(define-syntax-rule (make-list->enum _null? _car _cdr)
  (lambda (lst)
    (lambda (fn . init)
      (let next ((lst lst)
                 (state init))
        (if (_null? lst)
            (apply values state)
            (call-with-values
                (lambda () (apply fn (_car lst) state))
              (lambda (cont? . state+)
                (if cont?
                    (next (_cdr lst) state+)
                    (apply values state+))))))))) ;; (*)

(define list->enum   (make-list->enum null? car cdr))
(define stream->enum (make-list->enum stream-null? stream-car stream-cdr))
  

(define (seq->enum seq)
  (lambda (fn . init)
    (apply values
           (call/ec
            (lambda (done)
              (for/fold
                  ((state init)) ((el seq))
                  (call-with-values
                      (lambda () (apply fn el state))
                    (lambda (cont? . state+)
                      (if cont? state+ (done state+))))))))))

;; Converting an enumerator to a SRFI-45 stream.  This needs to invert
;; control of the enumerator.  Termination is not used (return #t).

(define (enum->stream enum)
  (reset
   (enum (lambda (el)
           (shift k (stream-cons el (k #t)))))
   stream-null))

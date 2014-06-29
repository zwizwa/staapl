#lang racket/base

(provide dfl-compose)
(require "dfl.rkt"
         (for-syntax racket/base
                     racket/match
                     "../tools/stx.rkt" ;; let-staged
                     "dfl.rkt"))

;; Compile DFL by first evaluating the syntax to a dependency graph
;; using the `dfl-graph' and passing this data structure and the
;; original syntax to `dfl-sort-graph' to produce a Scheme expression
;; that can be evaluated serially.  The staging is performed by
;; `let-staged'.

;; The construct(*) behaves as:
;;
;;    (let ((nodes (eval #'(dfl-graph formals body ...)))) ___)
;;
;; but doesn't use `eval'.  Instead it uses a 2nd macro stage to
;; trigger evaluation of the syntax form.


(define-syntax (dfl-compose stx)
  (syntax-case stx ()
    ((_ formals body ...)
     (let-staged ((nodes (dfl-graph formals body ...))) ;; (*)
         #`(dfl-sequence
            formals
            #,@(dfl-sort-graph
                nodes
                (syntax->list #'(body ...))))))))

(define (test)
  (define (debug x) (printf "DEBUG: ~x\n" x))
  (define (foo x) (add1 x))
  (define (bar x y) (* x y))
  (dfl-compose ((in1 in2) (out1) (tmp))
               (debug (in1) ())
               (foo (tmp) (out1))
               (bar (in1 in2) (tmp))))




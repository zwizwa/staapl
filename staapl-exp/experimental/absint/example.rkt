#lang racket/base
(require racket/unit
         racket/pretty
         "num.ss")


;; *** MODEL ***

(define-unit model@
  (import num^)
  (export run^)

  ;; Some internals.
  (define (fun a b) 
    (let ((x (+ a b))
          (y (- a b)))
      (* x y)))

  ;; Analysis function.
  (define (run a b)
    (fun (lit a) (lit b))))



;; *** ANALYSIS ***

;; This uses `num-run' which assumes the model unit implements the
;; run^ signature, defining a single function that performs the
;; computation/analysis/compilation/... implied by the num^ instance
;; it is invoked with.

;((num-run num-anf@  model@) 1 2)
;((num-run num-expr@ model@) 1 2)
;((num-run num-eval@ model@) 1 2)


;; Convenience on top of num-run.  Note that these are all macros
;; using the compile time information tagged to the identifiers.

(for ((f (num-runs (num-anf@
                    num-expr@
                    num-eval@)
                   model@)))
  (pretty-print (f 1 2)))


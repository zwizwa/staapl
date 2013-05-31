#lang scheme/base
(require scheme/unit
         scheme/pretty
         "num.ss"
         "statespace.ss"
         )


;; *** MODEL ***

(define-unit model@
  (import num^)
  (export run^)

  ;; Some internals.
  (define (frac x)
    (- x (floor x)))

;  (define-model (diff (s) (i) (o))
;    (let ((s+ (- i s)))
;      (model-update
;       ((s s+)
;        (o s+)))))

  (define (saw phase int frq pole)
    (let* ((phase+ (frac (+ frq phase)))
           (para   (* phase (- (lit 1) phase)))
           (int+   (+ int (* pole (- para int)))))
      (values phase+ int+)))

  ;; State space form.
  (define (s-saw pole)
    (state-space (((phase  int)  (frq))   ;; state  input
                  ((phase+ int+) (int+))  ;; state+ output
                  )
                 (saw (phase int frq pole) (phase+ int+))))

  ;; Analysis function.
  (define (run . args)
    (apply saw (map lit args))))




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
  (pretty-print
   (call-with-values (lambda () (f 1 2 3 4)) list)))



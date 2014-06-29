#lang racket/base

;; Implements a dynamical system as a self-modifying module
;; expression. The module body serves as state + storage for all the
;; functionality. It exports a simple function 'update' which takes an
;; input argument, and produces 2 values: the current output and the
;; updated code body.


(provide
 (rename-out (module-begin #%module-begin))
 (except-out (all-from-out racket/base) #%module-begin))

(require
 (for-syntax
  racket/base))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    ((_ (tick state) . forms)
     (let ((name
            (syntax-property
             stx 'enclosing-module-name)))
       #`(#%plain-module-begin
          (provide update)
          (define (update input)
            (let-values (((state+ output) (tick 'state input)))
              (values 
               output
               `(module #,name scat/quine
                  (tick ,state+) . forms))))
          . forms)))))

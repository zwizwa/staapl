#lang scheme/base
(provide (all-defined-out))
;; Toplevel namespace utilities

;; Rationale: for debugging you really want late binding.  This file
;; contains some utilities to use the toplevel namespace for 

;; Connect a parameter containing a function hook to a toplevel
;; identifier in the namespace.

(define (param-to-toplevel name param)
  (eval
   `(begin
      (define ,name (,param))
      (,param (lambda args (apply ,name args))))))
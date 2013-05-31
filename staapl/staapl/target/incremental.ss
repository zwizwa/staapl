#lang scheme/base

;; Target code produced during compilation (comp.ss) is registered
;; in-order, to implement Forth's allot-stack model for incremental
;; code upload.

;; Notes:
;;
;; * Target words and macros are also accessible in an
;;   unordered way directly from the target namespace using
;;   functionality from live/reflection.ss For non-incremental
;;   whole-program compilation, the reflective namespace access is
;;   sufficient: given the entry points (reset + interrupt vectors),
;;   all reachable code can be collected and assembled to be linked
;;   into a single binary image.
;;
;; * Because all reflective operations are built on top of Scheme's
;;   reflection operator 'eval', it is not possible to create a pure
;;   map from a source file to a (list-of target-word). The 'eval'
;;   operator is inherently imperative, updating a namespace
;;   object. For this reason, a hook is provided to tap into the
;;   instantiation process and perform necessary operations.

(require
 "rep.ss")

(provide
 register-code
 register-code-hook)


;; Called from 'forth-begin with a (list-of target-chain)

;; A target-chain is a target-word representing the entry point of a
;; list of consecutive target-word. This list can be obtained using
;; target-chain->list.  Different target-chain can be layed out in
;; memory independently.

;; A target-word is a wrapper around a chunk of machine code with a
;; single entry point.

(define (register-code set-of-target-chain [origin #f])
  (for-each (lambda (proc!) (proc! set-of-target-chain origin))
            (register-code-hook)))

;; The actions that need to be taken during instantiation of the code
;; are pluggable. This could for example assemble + upload the code.

(define register-code-hook (make-parameter '()))  ;; by default, ignore


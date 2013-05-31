#lang scheme/base

;; File moved to libprim.  This version is a snapshot
;; Tue Oct  6 17:56:40 CEST 2009

(require "../tools/mfile.ss"
         scheme/system
         scheme/runtime-path)

(define-runtime-path gnuc.h "gnuc.h")

(provide open-input-cpp)

;; External command wrappers.

(define (fsystem . args)
  (let ((cmd (apply format args)))
    (printf "> ~a\n" cmd)
    (system cmd)))

;; Explicitly naming inputs and outputs makes it easier to write
;; wrappers.
(define (cpp in out [args ""])
  (fsystem "cat ~s ~s | cpp ~a | grep -v '^#' >~s"
           (path->string gnuc.h) in args out))
           

(define (mfile-1->1 cmd)
  (lambda (mfile . args)
    (mdir-refs
     (with-mdir `(("in" . ,mfile))
                (lambda () (apply cmd "in" "out" args)))
     "out")))

(define c->cpp (mfile-1->1 cpp))


(define (open-input-cpp filename . args)
  (open-input-mfile
   (apply c->cpp (make-mfile filename) args)))

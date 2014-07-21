#lang racket/base
(require "../tools.rkt"
         racket/dict)
(provide (all-defined-out))

;; Reusable tools for writing machine emulators.


;; abstract register access

;; Convenient interface for register access
(define (register-fn reg [arg #f])
  (cond
   ((procedure? arg)
    ((register-read-modify-write reg) arg))
   (arg
    ((register-write reg) arg))
   (else
    ((register-read reg)))))

(define (register-print reg port write?)
  (write-string (format "#<register>") port))


(define-values
  (struct:register make-register register? register-ref register-set!)
  (begin
    (make-struct-type
     'register    ;; name-symbol
     #f           ;; super-struct-type
     3            ;; init-field-k
     0            ;; auto-field-k
     #f           ;; auto-v
     (list (cons prop:custom-write register-print))
     #f           ;; inspector or false
     register-fn  ;; word-run or 0
     )))

(define (register-read r) (register-ref r 0))
(define (register-write r) (register-ref r 1))
(define (register-read-modify-write r) (register-ref r 2))

#;(define-struct register
  (read
   write
   read-modify-write  ;; separate due to pre/post inc/dec on FSRs
   ))



;; if register access does not have side effects (see FSRs), just
;; implement rmw in terms of read & write
(define (make-rw-register read write)
  (define (read-modify-write update)
    (let ((v (update (read))))
      (write v)
      v))
  (make-register read write read-modify-write))

(define (make-r-register read)
  (make-rw-register read (lambda (v) (error 'read-only))))

(define (make-w-register read)
  (make-rw-register (lambda () (error 'write-only)) write))

(define (make-param-register param)
  (make-rw-register param param))

(define (make-ni-register tag)
  (define (ni . _) (error 'register-not-implemented "~s" tag))
  (make-register ni ni ni))


;; Represent a list of bool parameters as a 8-bit register interface.
(define (make-flags-register flags-params)
  (define flags (reverse flags-params))
  (define (read)
    (for/fold
        ((s 0))
        ((b (in-naturals))
         (f flags))
      (let ((v (f)))
        (unless (boolean? v)
          (error 'flag-type "~s" v))
        (bior s (<<< (bool->bit (f)) b)))))
  (define (write bits)
    (if (uninitialized? bits)
        (for ((f flags))
          (f (make-uninitialized)))
        (for ((b (in-naturals))
              (f flags))
          (f (bit->bool (band 1 (>>> bits b)))))))
  (make-rw-register read write))


;; Some macros for defining machine state parameters.
(define-struct uninitialized ())
(define (undefined-params lst)
  (apply values (for/list ((e lst))
                  (make-parameter (uninitialized)))))

(define-syntax-rule (params . ps)
  (define-values ps (undefined-params 'ps)))
(define-syntax-rule (flag-params (reg bits) ...)
  (begin
    (define-values bits (undefined-params 'bits)) ...
    (begin (define reg (make-flags-register (list . bits))) ...)
    ))




;; LATER: abstract memory access as well.

;; Convenient interface for memory access
(define (memory-fn reg addr [arg #f])
  (cond
   #;((procedure? arg)
      ((memory-read-modify-write reg) addr arg))
   (arg
    ((memory-write reg) addr arg))
   (else
    ((memory-read addr reg)))))

(define (memory-print reg port write?)
  (write-string (format "#<memory>") port))

(define-values
  (struct:memory make-memory memory? memory-ref memory-set!)
  (begin
    (make-struct-type
     'memory    ;; name-symbol
     #f           ;; super-struct-type
     2            ;; init-field-k
     0            ;; auto-field-k
     #f           ;; auto-v
     (list (cons prop:custom-write memory-print))
     #f           ;; inspector or false
     memory-fn  ;; word-run or 0
     )))


(define (memory-read  mem) (memory-ref mem 0))
(define (memory-write mem) (memory-ref mem 1))

(define (vector-memory vec)
  (make-memory
   (lambda (addr)     (vector-ref  vec addr))
   (lambda (addr val) (vector-set! vec addr val))))

;; Memory inspector wrapper.
(define (memory-inspect mem read-notify write-notify)
  (define (read addr)
    (let ((v ((memory-read mem) addr)))
      (read-notify addr v)
      v))
  (define (write addr vnew)
    (let ((vold ((memory-read mem) addr)))
      ((memory-write mem) addr vnew)
      (write-notify addr vold vnew)))
  (make-memory read write))

